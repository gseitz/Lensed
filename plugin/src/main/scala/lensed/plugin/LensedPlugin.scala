package lensed.plugin

import scala.tools.nsc
import nsc.Global
import nsc.plugins.{Plugin, PluginComponent}

class LensedPlugin(val global: Global) extends Plugin {
  import global._


  val LensedAnnotationClass = "lensed.annotation.lensed"

  val name = "lensed"
  val description = "support for the @lensed annotation"

  val components = List[PluginComponent](
    new GenerateSynthetics(this, global)
  )

}
