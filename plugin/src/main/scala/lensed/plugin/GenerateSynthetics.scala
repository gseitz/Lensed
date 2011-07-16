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

    private def pimpModuleDef(md: ModuleDef): Tree = {
      val impl = md.impl
      val mdClazz = md.symbol.moduleClass

      val cd = caseClasses(md.symbol.companionClass)

      val ccSymbol = cd.symbol

      val ccImpl = cd.impl.body

      def shouldMemberBeLenses_?(tree: Tree) = {
        val sym = tree.symbol
        sym.isCaseAccessor && sym.isParamAccessor && sym.isMethod
      }

      val lenses = ccImpl.filter(shouldMemberBeLenses_?).flatMap { ccMember =>

      //          println("processing member: " + ccMember)
        val memberSym = ccMember.symbol

        //          println("typetree: " + TypeTree(ccMember.symbol.tpe))

        val ccGetVal = ValDef(NoMods, newTermName("t"), REF(ccSymbol)/*TypeTree(cd.symbol.tpe)*/, EmptyTree)
        val ccSetVal = ValDef(NoMods, newTermName("t"), REF(ccSymbol), EmptyTree)
        val memSetVal = ValDef(NoMods, newTermName("m"), TypeTree(ccMember.symbol.tpe.resultType), EmptyTree)

        //          println("ccGetVal: " + ccGetVal)
        //          println("ccSetVal: " + ccSetVal)
        //          println("memSetVal: " + memSetVal)

        val lensApply = TypeApply(
          Select(Ident(newTermName("scalaz")) DOT newTermName("Lens"), newTermName("apply")),
          TypeTree(ccSymbol.tpe) :: TypeTree(ccMember.symbol.tpe) :: Nil)

        val memberSelect = Select(Ident(newTermName("t")), memberSym.name.toTermName)
        val copyMethod = fn(Ident(newTermName("t")), newTermName("copy"), Ident(newTermName("m")))

        val lensGet = Function(ccGetVal :: Nil, memberSelect)
        val lensSet = Function(ccSetVal :: memSetVal :: Nil, copyMethod)


        val rhs = Apply(lensApply, lensGet :: lensSet :: Nil)

        //          println("lensApply: " + lensApply)
        //          println("memberSelect: " + memberSelect)
        //          println("copyMethod: " + copyMethod)
        //          println("lensGet: " + lensGet)
        //          println("lensSet: " + lensSet)
        //          println("rhs: " + rhs)

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

        val lensVal = localTyper.typed {
          VAL(lensValSym) === rhs
        }
        val lensDef = localTyper.typed {
          DEF(lensDefSym) === Select(THIS(mdClazz), lensValName)
        }
        List(lensVal, lensDef)
      }

      val newImpl = treeCopy.Template(impl, impl.parents, impl.self, lenses ++ impl.body)
      treeCopy.ModuleDef(md, md.mods, md.name, newImpl)
    }


    def shouldLens(sym: Symbol) = {
      sym.annotations.foreach { ann=>
        println(ann.atp.typeSymbol)
        println(annotationClass)
      }
      sym.isCaseClass && sym.annotations.exists(_.atp.typeSymbol == annotationClass)

    }

    override def transform(tree: Tree): Tree = {
      val newTree = tree match {
        case cd @ ClassDef(_, _, _, _) if shouldLens(cd.symbol) =>
          //          println("found case class. classdef.symbol.tpe: " + cd.symbol.tpe)
          //          println("case class impl: classdef.tpe"+ cd.tpe)
          caseClasses +=  cd.symbol -> cd
          cd
        case md @ ModuleDef(mods, name, impl) if shouldLens(md.symbol.companionClass) =>
          println("Pimping " + md.symbol.tpe)
          pimpModuleDef(md)
        case _ => tree
      }
      super.transform(newTree)
    }
  }
}
