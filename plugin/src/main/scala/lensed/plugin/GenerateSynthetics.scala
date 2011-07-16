package lensed.plugin

import scala.tools._
import nsc.Global
import nsc.interpreter.Naming
import nsc.plugins.PluginComponent
import nsc.transform.{Transform, TypingTransformers}
import nsc.symtab.Flags._
import nsc.ast.TreeDSL
import nsc.typechecker.Namers


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

  val lensClass = definitions.getClass("scalaz.Lens")
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

    def generateLensSetFunction(ccSymbol: Symbol, ccMember: Tree, defaults: List[Tree]) = {
      val ccSetVal = ValDef(Modifiers(PARAM), newTermName("t"), REF(ccSymbol), EmptyTree)
      val memSetVal = ValDef(Modifiers(PARAM), newTermName("m"), TypeTree(ccMember.symbol.tpe.resultType), EmptyTree)
      Function(ccSetVal :: memSetVal :: Nil, generateLensSetBody(defaults, ccMember))
    }

    def generateLensGetFunction(ccSymbol: Symbol, memberSym: Symbol) = {
      val ccGetVal = ValDef(Modifiers(PARAM), newTermName("t"), REF(ccSymbol), EmptyTree)
      Function(ccGetVal :: Nil, Select(Ident(newTermName("t")), memberSym.name.toTermName))
    }

    private def pimpModuleDef(md: ModuleDef): Tree = {
      val impl = md.impl
      val mdClazz = md.symbol.moduleClass

      val cd = caseClasses(md.symbol.companionClass)

      val ccSymbol = cd.symbol

      val ccImpl = cd.impl.body

      val defaults = findDefaultDefsInOrder(cd)


      val lenses = ccImpl.filter(shouldMemberBeLensed_?).flatMap { ccMember =>

      //          log("processing member: " + ccMember)
        val memberSym = ccMember.symbol

        //          log("typetree: " + TypeTree(ccMember.symbol.tpe))

        //          log("ccGetVal: " + ccGetVal)
        //          log("ccSetVal: " + ccSetVal)
        //          log("memSetVal: " + memSetVal)

        val lensGet = generateLensGetFunction(ccSymbol, memberSym)
        val lensSet = generateLensSetFunction(ccSymbol, ccMember, defaults)


        val lensApply = TypeApply(
          Select(Ident(newTermName("scalaz")) DOT newTermName("Lens"), newTermName("apply")),
          TypeTree(ccSymbol.tpe) :: TypeTree(ccMember.symbol.tpe) :: Nil)
        val rhs = Apply(lensApply, lensGet :: lensSet :: Nil)

        //          log("lensApply: " + lensApply)
        //          log("memberSelect: " + memberSelect)
        //          log("copyMethod: " + copyMethod)
        //          log("lensGet: " + lensGet)
        //          log("lensSet: " + lensSet)
        //          log("rhs: " + rhs)

        val concreteLensType = appliedType(lensClass.tpe, ccSymbol.tpe :: ccMember.symbol.tpe.resultType :: Nil)

        val lensValName = memberSym.name.toTermName.append("0")

        val lensValSym = mdClazz.newValue(mdClazz.pos.focus, lensValName)
        lensValSym setFlag (SYNTHETIC | PRIVATE)
        lensValSym setInfo  concreteLensType
        mdClazz.info.decls enter lensValSym

        val lensDefSym = mdClazz.newMethod(mdClazz.pos.focus, ccMember.symbol.name.toTermName)
        lensDefSym setFlag (SYNTHETIC)
        lensDefSym setInfo MethodType(Nil, concreteLensType)
        mdClazz.info.decls enter lensDefSym

        val lensVal = localTyper.typed { VAL(lensValSym) === rhs }
        val lensDef = localTyper.typed { DEF(lensDefSym) === Select(THIS(mdClazz), lensValName) }
        List(lensVal, lensDef)
      }

      val newImpl = treeCopy.Template(impl, impl.parents, impl.self, lenses ++ impl.body)
      treeCopy.ModuleDef(md, md.mods, md.name, newImpl)
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
