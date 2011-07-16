import sbt._
import scala.Seq._

class LensedPluginProject(info: ProjectInfo) extends ParentProject(info) {

  lazy val theAnnotation = project("annotation", "Annotation", new TheAnnotation(_))
  lazy val thePlugin = project("plugin", "The plugin", new ThePlugin(_), theAnnotation)
  lazy val examples = project("examples", "Examples", new Examples(_))

  class TheAnnotation(info: ProjectInfo) extends DefaultProject(info)

  class ThePlugin(info: ProjectInfo) extends DefaultProject(info) {
    override def compileOptions = compileOptions(
      "-deprecation",
      "-unchecked"
    ) ++ super.compileOptions

    override def filterScalaJars = false

    override def unmanagedClasspath = 
      super.unmanagedClasspath --- buildLibraryJar --- buildCompilerJar

    //Dependencies
	
    val scalac = "org.scala-lang" % "scala-compiler" % buildScalaVersion withSources()
    val scalalib = "org.scala-lang" % "scala-library" % buildScalaVersion withSources()
    val unit = "junit" % "junit" % "4.8.2" % "test" withSources()
    val scalaz = "org.scalaz" %% "scalaz-core" % "6.0.1" withSources()
  }
  
  class Examples(info: ProjectInfo) extends ParentProject(info){
    lazy val simple = project("simple", "Simple Example", new ExampleProject(_), theAnnotation, thePlugin)
    lazy val usage = project("usage", "Usage Example", new Usage(_), simple)

    class Usage(info: ProjectInfo) extends DefaultProject(info) {
      val scalaz = "org.scalaz" %% "scalaz-core" % "6.0.1" withSources()
    }

    class ExampleProject(info: ProjectInfo) extends DefaultProject(info) {
//      override def compileClasspath = thePlugin.runClasspath
      override def compileOptions = compileOptions(
        "-Xplugin:./plugin/target/scala_" + buildScalaVersion + "/the-plugin_" + buildScalaVersion + "-" + version + ".jar",
        "-verbose",
//        "-usejavacp",
//        "-nobootcp",
//        "-Xplugin:plugin/src/test/stub-jar/dynamic-mixin-stub.jar",
        "-Xplugin-require:lensed",
//        "-Ybrowse:generatesynthetics",
//        "-Ybrowse:lazyvals",
//        "-Xprint:lazyvals",
        "-Ylog:generatesynthetics",
//        "-Ylog:lambdalift",
        "-Ydebug",
        "-Yshow-syms"
//        "-Ycheck:generatesynthetics"
//        "-Ycheck:lazyvals"
//        "-Ybrowse:lazyvals"
//        "-Yshow-trees"
//        "-Xplugin-list"
//        "-Xshow-phases"
      ) ++ super.compileOptions

      val scalaz = "org.scalaz" %% "scalaz-core" % "6.0.1" withSources()
    }
  }
}
