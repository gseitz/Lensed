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

  val sequence = new AtomicLong(0)


  def newTransformer(unit: CompilationUnit) = new LensedTransformer(unit)

  class LensedTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    import CODE._

    private def findDefaultDefsInOrder(caseClass: ClassDef) = {
      def isDefaultDef(tree: Tree) = {
        val sym = tree.symbol
        sym.isSynthetic && sym.isMethod && sym.name.toString.startsWith("copy$default$")
      }

      val defaults = for (member <- caseClass.impl.body if isDefaultDef(member)) yield member
      defaults reverse
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

    def generateLensSetFunction(caseClassType: Type, caseClassMember: Tree, defaults: List[Tree]) = {
      val caseClassSetVal = ValDef(Modifiers(PARAM), newTermName("t"), TypeTree(caseClassType), EmptyTree)
      val memberSetVal = ValDef(Modifiers(PARAM), newTermName("m"), TypeTree(caseClassType.computeMemberType(caseClassMember.symbol).resultType), EmptyTree)
      Function(caseClassSetVal :: memberSetVal :: Nil, generateLensSetBody(defaults, caseClassMember))
    }

    def generateLensGetFunction(caseClassType: Type, memberSym: Symbol) = {
      val caseClassGetVal = ValDef(Modifiers(PARAM), newTermName("t"), TypeTree(caseClassType), EmptyTree)
      Function(caseClassGetVal :: Nil, Select(Ident(newTermName("t")), memberSym.name.toTermName))
    }

    def lensType(typeParams: Type*) = appliedType(lensClass.tpe, typeParams.toList)

    def typedTypeRef(symbol: Symbol, typeParams: List[Type]) = {
      if (typeParams.isEmpty) symbol.tpe
      else typeRef(symbol.typeConstructor.prefix, symbol, typeParams)
    }

    def typeParamTypeSymbols(parent: Symbol, tparams: List[Symbol]) = tparams.map { sym =>
      val bounds = sym.tpe.bounds
      val param = parent.newTypeParameter(parent.pos.focus, sym.name.toTypeName.append(sequence.getAndIncrement.toString))
      param setFlag (DEFERRED | PARAM)
      param setInfo TypeBounds(bounds.lo, bounds.hi)
    }


    def generateLenses(moduleClass: Symbol, caseClass: Symbol, caseClassTypeParams: List[Symbol], members: List[Tree], defaultMemberOrder: List[Tree]): List[Tree] = {

      members.map { member =>

      //          log("processing member: " + member)
        val memberSym = member.symbol

        //          log("typetree: " + TypeTree(member.symbol.tpe))

        //          log("ccGetVal: " + ccGetVal)
        //          log("ccSetVal: " + ccSetVal)
        //          log("memSetVal: " + memSetVal)


        //          log("lensApply: " + lensApply)
        //          log("memberSelect: " + memberSelect)
        //          log("copyMethod: " + copyMethod)
        //          log("lensGet: " + lensGet)
        //          log("lensSet: " + lensSet)
        //          log("rhs: " + rhs)
        val lensDefSym = moduleClass.newMethod(moduleClass.pos.focus, memberSym.name.toTermName)

        val lensDefTypeParams = typeParamTypeSymbols(lensDefSym, caseClassTypeParams)

        val caseClassType = typedTypeRef(caseClass, lensDefTypeParams.map(_.tpe))

        //        log("ccTpe: " + ccTpe)
        //        log("ccTpe.typeSymbol: " + ccTpe.typeSymbol)
        //        log("lensDefTypeParams: " + lensDefTypeParams.mkString(", "))

        val concreteLensType = lensType(caseClassType, caseClassType.computeMemberType(memberSym).finalResultType)
        //        log("concreteLensType: " + concreteLensType)


        val lensGet = generateLensGetFunction(caseClassType, memberSym)
        val lensSet = generateLensSetFunction(caseClassType, member, defaultMemberOrder)

        val lensApply = TypeApply(
          Select(Ident(newTermName("scalaz")) DOT newTermName("Lens"), newTermName("apply")),
          TypeTree(caseClassType) :: TypeTree(caseClassType.computeMemberType(member.symbol)) :: Nil)

        val rhs = Apply(lensApply, lensGet :: lensSet :: Nil)

        val lensDefType =
          if(lensDefTypeParams.isEmpty) NullaryMethodType(concreteLensType)
          else typeFun(lensDefTypeParams, NullaryMethodType(concreteLensType))

        //        log("lensDefType: " + lensDefType)

        lensDefSym setInfo lensDefType
        lensDefSym setFlag (SYNTHETIC)
        moduleClass.info.decls enter lensDefSym

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

        DEF(lensDefSym) === rhs
      }
    }


    def generateCaseClassW(moduleClass: Symbol, caseClass: Symbol, caseClassTypeParams: List[Symbol], caseClassMembers: List[Symbol]) = {
      val caseClassWName = caseClass.name.append("W").toTypeName

      val caseClassW = moduleClass.newClass(moduleClass.pos.focus, caseClassWName)

      val lensParamType = {
        val param = caseClassW.newTypeParameter(caseClassW.pos.focus, newTypeName("L").append(sequence.getAndIncrement.toString))
        param setFlag (DEFERRED | PARAM)
        param setInfo (localTyper typed { TypeTree(TypeBounds.empty) }).tpe
        param
      }

      val caseClassParamTypeSymbols = typeParamTypeSymbols(caseClassW, caseClassTypeParams)

      val caseClassWParamTypeSymbols = lensParamType :: caseClassParamTypeSymbols

      caseClassW setInfo polyType(
        caseClassWParamTypeSymbols.map(_.tpe.typeSymbol),
        ClassInfoType(
          ObjectClass.tpe :: ScalaObjectClass.tpe :: Nil,
          new Scope,
          caseClassW))

      moduleClass.info.decls enter caseClassW

      val typedCaseClass = typedTypeRef(caseClass, caseClassParamTypeSymbols.map(_.tpe))

      val constrParamSym = caseClassW.newValue(caseClassW.pos.focus, newTermName("l"))
      constrParamSym setInfo lensType(lensParamType.tpe, typedCaseClass)
      constrParamSym setFlag (PARAMACCESSOR | PRIVATE)
      caseClassW.info.decls enter constrParamSym

      ClassDef(caseClassW,
        Template(
          caseClassW.info.parents map TypeTree,
          if (caseClassW.thisSym == caseClassW || phase.erasedTypes) emptyValDef else ValDef(caseClassW.thisSym),
          NoMods,
          List(List(ValDef(constrParamSym, EmptyTree))),
          List(List()),
          caseClassMembers map { memberSym =>
            val defName = memberSym.name.toTermName

            val memberResultType = typedCaseClass.computeMemberType(memberSym).finalResultType

            val memberDefSym = caseClassW.newMethod(caseClassW.pos.focus, defName)
            memberDefSym setFlag (METHOD)
            memberDefSym setInfo NullaryMethodType(typedTypeRef(lensClass, lensParamType.tpe :: memberResultType :: Nil))
            caseClassW.info.decls enter memberDefSym

            DefDef(memberDefSym,
              Apply(
                TypeApply(
                  Select(
                    Select(This(caseClassWName), newTermName("l")),
                    newTermName("andThen")
                  ),
                  TypeTree(memberResultType) :: Nil
                ),
                if (caseClassTypeParams.isEmpty)
                  Select(Ident(moduleClass.name.toTermName), memberSym.name) :: Nil
                else
                  TypeApply(
                    Select(Ident(moduleClass.name.toTermName), memberSym.name),
                  caseClassParamTypeSymbols.map(t=>TypeTree(t.tpe))
                  ) :: Nil
              )
            )
          },
          NoPosition
        )
      )
    }

    def generateImplicitDef(moduleClass: Symbol, caseClass: Symbol, caseClassTypeParams: List[Symbol], caseClassW: Symbol)  = {

      val implicitDefSym = moduleClass.newMethod(moduleClass.pos.focus, newTermName("lens2").append(caseClassW.name.toString))

      val implicitDefTypeParam = implicitDefSym.newTypeParameter(implicitDefSym.pos.focus, newTypeName("A"))
      implicitDefTypeParam setFlag (DEFERRED | PARAM)
      implicitDefTypeParam setInfo TypeBounds.empty

      val caseClassTypeParamSymbols = typeParamTypeSymbols(implicitDefSym, caseClassTypeParams)

      val typedCaseClass = typedTypeRef(caseClass, caseClassTypeParamSymbols.map(_.tpe))

      val implicitDefParamType = TypeRef(lensClass.tpe.prefix, lensClass, implicitDefTypeParam.tpe :: typedCaseClass :: Nil)

      implicitDefSym setFlag (IMPLICIT)
      implicitDefSym setInfo polyType(
        implicitDefTypeParam :: caseClassTypeParamSymbols,
        MethodType(
          implicitDefSym.newSyntheticValueParams(implicitDefParamType :: Nil),
          TypeRef(moduleClass.thisType, caseClassW, (implicitDefTypeParam :: caseClassTypeParamSymbols).map(_.tpe))
        )
      )

      moduleClass.info.decls enter implicitDefSym
      moduleClass.info.decls enter implicitDefTypeParam

     DEF(implicitDefSym) === NEW(TypeTree(typeRef(moduleClass.thisType, caseClassW, (implicitDefTypeParam :: caseClassTypeParamSymbols).map(_.tpe))), implicitDefSym ARG 0)
    }



    private def extendModuleDef(md: ModuleDef): Tree = {
      val moduleClass = md.symbol.moduleClass

      val cd = caseClasses(md.symbol.companionClass)
      val caseClass = cd.symbol
      val caseClassImpl = cd.impl.body
      val caseClassTypeParams = cd.tparams.map(_.symbol)

      val defaults = findDefaultDefsInOrder(cd);

//      log("ClassDef: " + cd)
//      log("ClassDef.symbol: " + cd.symbol)
//      log("ClassDef.tparams: " + ccTypeParams.mkString(", "))

      val lensTargetMembers = caseClassImpl.filter(shouldMemberBeLensed_?)


      val lenses = generateLenses(moduleClass, caseClass, caseClassTypeParams, lensTargetMembers, defaults).map {
        lens => localTyper typed { lens }
      }
      

      val owner0 = localTyper.context1.enclClass.owner
      localTyper.context1.enclClass.owner = moduleClass


      val caseClassW = localTyper typed {
        generateCaseClassW(moduleClass, caseClass, caseClassTypeParams, lensTargetMembers.map(_.symbol))
      }

      val implicitDef = localTyper typed {
        generateImplicitDef(moduleClass, caseClass, caseClassTypeParams, caseClassW.symbol)
      }

      localTyper.context1.enclClass.owner = owner0



      val impl = md.impl
      val newImpl2 = treeCopy.Template(impl, impl.parents, impl.self, lenses ::: caseClassW :: implicitDef :: impl.body)
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
          extendModuleDef(md)
        case _ => tree
      }
      super.transform(newTree)
    }
  }
}
