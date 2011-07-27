package lensed.plugin

import scala.tools._
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.{Transform, TypingTransformers}
import nsc.symtab.Flags._
import nsc.ast.TreeDSL
import java.util.concurrent.atomic.AtomicLong


class GenerateSynthetics(plugin: LensedPlugin, val global: Global) extends PluginComponent
with Transform
with TypingTransformers
with TreeDSL
{
  import global._
  import definitions._

  val caseClasses = collection.mutable.HashMap[global.Symbol, ClassDef]()

  val runsAfter = List("typer")
  val phaseName = "generatesynthetics"

  val lensClass: Symbol = definitions.getClass("scalaz.Lens")
  val annotationClass = definitions.getClass("lensed.annotation.lensed")

  def newTransformer(unit: CompilationUnit) = new LensedTransformer(unit)

  class LensedTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    import CODE._

    private def toIdent(x: DefTree) = Ident(x.name) setPos x.pos.focus

    private def findDefaultDefsInOrder(caseClass: ClassDef) = {
      def isDefaultDef(tree: Tree) = {
        val sym = tree.symbol
        sym.isSynthetic && sym.isMethod && sym.name.toString.startsWith("copy$default$")
      }

      val defaults = for (member <- caseClass.impl.body if isDefaultDef(member)) yield member
      defaults.sortBy(_.symbol.name.toTermName.toString)
    }

    private def findMemberPosition(defaults: List[Tree], member: Tree) = {
      defaults.indexWhere { t =>
        t.children.exists {
          case Select(_, name) => name.toString == member.symbol.name.toString
          case _ => false
        }
      }
    }

    def shouldMemberBeLensed_?(tree: Tree) = {
      val sym = tree.symbol
      sym.isCaseAccessor && sym.isParamAccessor && sym.isMethod
    }

    def generateLensSetBody(defaults: List[Tree], ccMember: Tree) = {
      val appliedDefaults = defaults.map {
        default =>
          Select(Ident(newTermName("t")), default.symbol)
      }
      val position = findMemberPosition(defaults, ccMember)
      val copyParams = appliedDefaults.updated(position, Ident(newTermName("m")))
      val internalCopyVals = copyParams.zipWithIndex.map {
        case (tree, i) => ValDef(NoMods, "x$" + i, TypeTree(tree.tpe), tree)
      }
      val paramNames = internalCopyVals.zipWithIndex.map {
        p => Ident(newTermName("x$" + p._2))
      }
      val copyMethod = fn(Ident(newTermName("t")), newTermName("copy"), paramNames: _*)
      val copyBlock = Block((internalCopyVals :+ copyMethod): _*)
      copyBlock
    }

    def generateLensSetFunction(ccType: Type, ccMember: Tree, defaults: List[Tree]) = {
      val ccSetVal = ValDef(Modifiers(PARAM), newTermName("t"), TypeTree(ccType), EmptyTree)
      val memSetVal = ValDef(Modifiers(PARAM), newTermName("m"), TypeTree(ccType.computeMemberType(ccMember.symbol).resultType), EmptyTree)
      Function(ccSetVal :: memSetVal :: Nil, generateLensSetBody(defaults, ccMember))
    }

    def generateLensGetFunction(ccType: Type, memberSym: Symbol) = {
      val ccGetVal = ValDef(Modifiers(PARAM), newTermName("t"), TypeTree(ccType), EmptyTree)
      Function(ccGetVal :: Nil, Select(Ident(newTermName("t")), memberSym.name.toTermName))
    }

    def lensType(typeParams: Type*) = appliedType(lensClass.tpe, typeParams.toList)

    def classType(symbol: Symbol, typeParams: List[Type]) =
      if (typeParams.isEmpty) symbol.tpe
      else typeRef(symbol.typeConstructor.prefix, symbol, typeParams)

    val symSeq = new AtomicLong(0)

    def typeParamTypeSymbols(parent: Symbol, tparams: List[TypeDef]) = tparams.map { tp =>
      val bounds = tp.symbol.tpe.bounds
      val param = parent.newTypeParameter(parent.pos.focus, newTypeName(tp.name.toString).append(symSeq.getAndIncrement.toString))
      param setFlag (DEFERRED | PARAM)
      param setInfo TypeBounds(bounds.lo, bounds.hi)
    }

    private def pimpModuleDef(md: ModuleDef): Tree = {
      val impl = md.impl
      val mdClazz = md.symbol.moduleClass

      val cd = caseClasses(md.symbol.companionClass)
      val ccSymbol = cd.symbol
      val ccImpl = cd.impl.body
      val ccTypeParams = cd.tparams

//      log("ClassDef: " + cd)
//      log("ClassDef.symbol: " + cd.symbol)
//      log("ClassDef.tparams: " + ccTypeParams.mkString(", "))

      val defaults = findDefaultDefsInOrder(cd);

      
//      val ccW = {
//
//        val ccClazzName = newTypeName(mdClazz.nameString + "W")
//        val ccClazzW = mdClazz.newClass(mdClazz.pos.focus, ccClazzName)
//        val ccClazzWTParam = TypeDef(Modifiers(DEFERRED | PARAM), newTypeName("LensA"), Nil, TypeBoundsTree(TypeTree(NothingClass.tpe), TypeTree(AnyClass.tpe)))
//
//        val ccClazzWTypes = typeParamTypeSymbols(ccClazzW, ccClazzWTParam :: ccTypeParams)
//
//        val ccType = classType(ccSymbol, ccTypeParams.map(_.tpe))
//        val concreteLensType = lensType(ccClazzWTParam.symbol.tpe, ccType)
//
//        val lensDefType =
//          if(ccClazzWTypes.isEmpty) MethodType(Nil, concreteLensType)
//          else typeFun(ccClazzWTypes, concreteLensType)
//
//
//
//        val lenses: List[Tree] = ccImpl.filter(shouldMemberBeLensed_?).flatMap { ccMember =>
//          val ccClazzWTypes = typeParamTypeSymbols(ccClazzW, ccClazzWTParam :: ccTypeParams)
//          val ccType = classType(ccClazzW, ccClazzWTypes.map(_.tpe))
//          val ccMemberFinalResultType = ccType.computeMemberType(ccMember.symbol).finalResultType
//
//          val lensDefSym = ccClazzW.newMethod(ccClazzW.pos.focus, ccMember.symbol.name.toTermName)
//          val compose =
//            Apply(
//              TypeApply(
//                Select(Select(This(ccClazzW), newTermName("l")), newTermName("andThen")),
//                TypeTree(ccClazzWTParam.tpe) :: TypeTree(ccMemberFinalResultType) :: Nil),
//              TypeApply(
//                Select(REF(mdClazz), ccMember.symbol.name),
//                TypeTree(ccMemberFinalResultType) :: Nil) :: Nil
//            )
//          lensDefSym setInfo lensDefType
//          lensDefSym setFlag (SYNTHETIC)
//
//          val result = List(localTyper typed { DefDef(lensDefSym, compose) })
//          ccClazzW.info.decls enter lensDefSym
//          result
//        }
//
//        val implicitCcW = {
//          val implicitDefTypeParams = typeParamTypeSymbols(ccClazzW, ccClazzWTParam :: ccTypeParams)
//          val ccClazzWType = classType(ccClazzW, implicitDefTypeParams.map(_.tpe))
//          val concreteccWType = appliedType(ccClazzW.tpe, ccClazzWTParam.symbol.tpe :: ccClazzWType :: Nil)
//          val implicitDefType = typeFun(implicitDefTypeParams, concreteccWType)
//          val implicitDefSym = mdClazz.newMethod(mdClazz.pos.focus, newTermName("ccw"))
//          implicitDefSym setInfo implicitDefType
//          implicitDefSym setFlag (SYNTHETIC | IMPLICIT)
//
//          mdClazz.info.decls enter implicitDefSym
//
//          localTyper typed {
//            DefDef(
//              implicitDefSym,
//              List(List(ValDef(Modifiers(PARAM), newTermName("l"), TypeTree(concreteccWType), EmptyTree))),
//              Apply(
//                Select(
//                  New(TypeTree(ccClazzWType)),
//                  nme.CONSTRUCTOR
//                ),
//                Ident(newTermName("l")) :: Nil
//              )
//            )
//          }
//        }
//        mdClazz.info.decls enter ccClazzW
//
//        ClassDef(ccClazzW,
//          Template(List[Tree](),
//            emptyValDef: ValDef,
//            NoMods,
//            List(List(ValDef(NoMods, newTermName("l"), TypeTree(ccType), EmptyTree))),
//            List(List[Tree]()),
//            lenses ::: implicitCcW :: Nil,
//            ccClazzW.pos.focus
//          )
//        )
//      }



      val lenses = ccImpl.filter(shouldMemberBeLensed_?).flatMap { ccMember =>

      //          log("processing member: " + ccMember)
        val memberSym = ccMember.symbol

        //          log("typetree: " + TypeTree(ccMember.symbol.tpe))

        //          log("ccGetVal: " + ccGetVal)
        //          log("ccSetVal: " + ccSetVal)
        //          log("memSetVal: " + memSetVal)


        //          log("lensApply: " + lensApply)
        //          log("memberSelect: " + memberSelect)
        //          log("copyMethod: " + copyMethod)
        //          log("lensGet: " + lensGet)
        //          log("lensSet: " + lensSet)
        //          log("rhs: " + rhs)
        val lensDefSym = mdClazz.newMethod(mdClazz.pos.focus, ccMember.symbol.name.toTermName)

        val lensDefTypeParams = typeParamTypeSymbols(lensDefSym, ccTypeParams)

        val ccTpe = classType(ccSymbol, lensDefTypeParams.map(_.tpe))

//        log("ccTpe: " + ccTpe)
//        log("ccTpe.typeSymbol: " + ccTpe.typeSymbol)
//        log("lensDefTypeParams: " + lensDefTypeParams.mkString(", "))



        val concreteLensType = lensType(ccTpe, ccTpe.computeMemberType(ccMember.symbol).finalResultType)
//        log("concreteLensType: " + concreteLensType)


        val lensGet = generateLensGetFunction(ccTpe, memberSym)
        val lensSet = generateLensSetFunction(ccTpe, ccMember, defaults)

        val lensApply = TypeApply(
          Select(Ident(newTermName("scalaz")) DOT newTermName("Lens"), newTermName("apply")),
          TypeTree(ccTpe) :: TypeTree(ccTpe.computeMemberType(ccMember.symbol)) :: Nil)

        val rhs = Apply(lensApply, lensGet :: lensSet :: Nil)



        val lensDefType =
          if(lensDefTypeParams.isEmpty) NullaryMethodType(concreteLensType)
          else typeFun(lensDefTypeParams, NullaryMethodType(concreteLensType))

//        log("lensDefType: " + lensDefType)

        lensDefSym setInfo lensDefType
        lensDefSym setFlag (SYNTHETIC)
        mdClazz.info.decls enter lensDefSym

//        log("rhs: " + rhs)
//        log("lensDefSym: " + lensDefSym)

        // not using the val for now
//        val lensValName = memberSym.name.toTermName.append("0")
//        val lensValSym = mdClazz.newValue(mdClazz.pos.focus, lensValName)
//        lensValSym setFlag (SYNTHETIC | PRIVATE)
//        lensValSym setInfo  concreteLensType
//        mdClazz.info.decls enter lensValSym
//        val lensVal = localTyper.typed { VAL(lensValSym) === rhs }
//        val lensDef = localTyper.typed { DEF(lensDefSym) === Select(THIS(mdClazz), lensValName) }
        
        val lensDef = localTyper.typed { DEF(lensDefSym) === rhs }

//        List(lensVal, lensDef)
        List(lensDef)

      }


      val owner0 = localTyper.context1.enclClass.owner
      localTyper.context1.enclClass.owner = mdClazz



      val caseClassW: ClassDef = {
        val caseClassWName = ccSymbol.name.append("W").toTypeName
//        val = TypeDef(NoMods, newTypeName("PLens"), Nil, TypeBoundsTree(TypeTree(NothingClass.tpe), TypeTree(AnyClass.tpe)))

        val caseClassWSym = mdClazz.newClass(mdClazz.pos.focus, caseClassWName)
//        val PLensParamTypes = typeParamTypeSymbols(caseClassWSym, TypeDef(Modifiers(DEFERRED | PARAM), newTypeName("PLens"), Nil, TypeBoundsTree(TypeTree(NothingClass.tpe), TypeTree(AnyClass.tpe))) :: Nil)

        val PLensParamType = {
          val param = caseClassWSym.newTypeParameter(caseClassWSym.pos.focus, newTypeName("PLens"))
          param setFlag (DEFERRED | PARAM)
          param setInfo (localTyper typed { TypeTree(TypeBounds.empty) }).tpe
          param
        }

        val ccParamTypes = typeParamTypeSymbols(caseClassWSym, ccTypeParams)

        val ccwParamTypes = PLensParamType :: ccParamTypes

//        val ccType = classType(caseClassWSym, ccwParamTypes.map(_.tpe))

        log("PLensParamTypes: " + ccwParamTypes.mkString(", "))
        log("typeFun: " + typeFun(ccwParamTypes, caseClassWSym.tpe))
//        log("ccType: " + ccType)


        caseClassWSym setInfo polyType(
          ccwParamTypes.map(_.tpe.typeSymbol),
          ClassInfoType(
            ObjectClass.tpe :: ScalaObjectClass.tpe :: Nil,
            new Scope,
            caseClassWSym))
//        ccwParamTypes.foreach(pt=> caseClassWSym.info.decls enter pt)
        
        mdClazz.info.decls enter caseClassWSym

//        caseClassWSym.newConstructor(caseClassWSym.pos.focus)

        log("caseClassWSym: " + caseClassWSym)
        log("caseClassWSym.info: " + caseClassWSym.info)
//        log("appliedType: " + appliedType(caseClassWSym.tpe, PLensParamTypes.map(_.tpe)))
        val constrParamSym = caseClassWSym.newValue(caseClassWSym.pos.focus, newTermName("l"))
        constrParamSym setInfo lensType(PLensParamType.tpe, classType(ccSymbol, ccParamTypes.map(_.tpe)))
        constrParamSym setFlag (PARAMACCESSOR | PRIVATE)
        caseClassWSym.info.decls enter constrParamSym

//        case class ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template)
        ClassDef(caseClassWSym,
          Template(
            caseClassWSym.info.parents map TypeTree,
            if (caseClassWSym.thisSym == caseClassWSym || phase.erasedTypes) emptyValDef else ValDef(caseClassWSym.thisSym),
            NoMods,
            List(List(ValDef(constrParamSym, EmptyTree))),
            List(List()),
          ccImpl.filter(shouldMemberBeLensed_?).flatMap { ccMember =>
              val memberSym = ccMember.symbol
              val defName = memberSym.name.toTermName
              val valName = memberSym.name.toTermName.append("0")

              val appliedT = classType(ccSymbol,ccParamTypes.map(_.tpe))

//              val memberValSym = caseClassWSym.newValue(caseClassWSym.pos.focus, valName)
//              memberValSym setFlag (PRIVATE | LOCAL)
//              memberValSym setInfo lensType(PLensParamType.tpe, appliedT.computeMemberType(memberSym).finalResultType)
//              caseClassWSym.info.decls enter memberValSym

              val memberDefSym = caseClassWSym.newMethod(caseClassWSym.pos.focus, defName)
              memberDefSym setFlag (METHOD)
//              memberDefSym setInfo polyType(ccParamTypes, NullaryMethodType(classType(lensClass, PLensParamType.tpe :: appliedT.computeMemberType(memberSym).finalResultType :: Nil)))
              memberDefSym setInfo NullaryMethodType(classType(lensClass, PLensParamType.tpe :: appliedT.computeMemberType(memberSym).finalResultType :: Nil))
              caseClassWSym.info.decls enter memberDefSym

//              ValDef(memberValSym,
//                Apply(
//                  TypeApply(
//                    Select(
//                      Select(This(caseClassWName), newTermName("l")),
//                      newTermName("andThen")
//                    ),
////                    TypeTree(ccType.computeMemberType(memberSym).finalResultType) :: Nil
//                    ccParamTypes.map(t=>TypeTree(t.tpe))
//                  ),
//                  if (ccTypeParams.isEmpty)
//                    Select(Ident(mdClazz.name.toTermName), memberSym.name) :: Nil
//                  else
//                    TypeApply(
//                      Select(Ident(mdClazz.name.toTermName), memberSym.name),
////                      ccType.computeMemberType(memberSym).finalResultType)::Nil
//                    ccParamTypes.map(t=>TypeTree(t.tpe))
//                    ) :: Nil
//                )
//              ) ::
              DefDef(memberDefSym,
//                Select(This(caseClassWSym), valName)
                Apply(
                  TypeApply(
                    Select(
                      Select(This(caseClassWName), newTermName("l")),
                      newTermName("andThen")
                    ),
                    TypeTree(appliedT.computeMemberType(memberSym).finalResultType) :: Nil
//                    ccParamTypes.map(t=>TypeTree(t.tpe))
                  ),
                  if (ccTypeParams.isEmpty)
                    Select(Ident(mdClazz.name.toTermName), memberSym.name) :: Nil
                  else
                    TypeApply(
                      Select(Ident(mdClazz.name.toTermName), memberSym.name),
//                      ccType.computeMemberType(memberSym).finalResultType)::Nil
                    ccParamTypes.map(t=>TypeTree(t.tpe))
                    ) :: Nil
                )
              ) :: Nil
            },
          NoPosition

          )
        )
        

//        )



      }

      println("caseClassW: " + caseClassW)

//      val typedCaseClassW = caseClassW// localTyper typed { caseClassW }
      val typedCaseClassW =  localTyper typed { caseClassW }



      val implicitDef = {

        val idSym = mdClazz.newMethod(mdClazz.pos.focus, newTermName("lens2").append(caseClassW.name.toString))

        val idTypeParam = idSym.newTypeParameter(idSym.pos.focus, newTypeName("A"))
        idTypeParam setInfo TypeBounds.empty //(localTyper typed { TypeTree(TypeBounds.empty) })

        val ccTypeParamSymbols = typeParamTypeSymbols(idSym, ccTypeParams)

        val ccType = classType(ccSymbol, ccTypeParamSymbols.map(_.tpe))

        val idParamType = TypeRef(lensClass.tpe.prefix, lensClass, idTypeParam.tpe :: ccType :: Nil)

        idSym setFlag (IMPLICIT)
        idSym setInfo polyType(
          idTypeParam :: ccTypeParamSymbols,
          MethodType(
            idSym.newSyntheticValueParams(idParamType :: Nil),
            TypeRef(mdClazz.thisType, typedCaseClassW.symbol, idTypeParam.tpe :: ccTypeParamSymbols.map(_.tpe))
          )
        )

        mdClazz.info.decls enter idSym
        mdClazz.info.decls enter idTypeParam

        localTyper typed {
          DEF(idSym) === NEW(TypeTree(typeRef(mdClazz.thisType, typedCaseClassW.symbol, idTypeParam.tpe :: ccTypeParamSymbols.map(_.tpe))), idSym ARG 0)
        }
      }

      localTyper.context1.enclClass.owner = owner0



      val newImpl2 = treeCopy.Template(impl, impl.parents, impl.self, lenses ::: typedCaseClassW :: implicitDef :: impl.body)
      treeCopy.ModuleDef(md, md.mods, md.name, newImpl2)
    }


    def shouldLens(sym: Symbol) = sym.isCaseClass && sym.annotations.exists(_.atp.typeSymbol == annotationClass)

    override def transform(tree: Tree): Tree = {
      val newTree = tree match {
        case cd @ ClassDef(_, _, _, _) if shouldLens(cd.symbol) =>
          //          log("found case class. classdef.symbol.tpe: " + cd.symbol.tpe)
          //          log("case class impl: classdef.tpe"+ cd.tpe)
          caseClasses +=  cd.symbol -> cd
          cd
        case md @ ModuleDef(mods, name, impl) if shouldLens(md.symbol.companionClass) =>
          log("Pimping " + md.symbol.tpe)
          pimpModuleDef(md)
        case _ => tree
      }
      super.transform(newTree)
    }
  }
}
