import sbt._
import Keys._

object LensedBuild extends Build {

  object BuildSettings {
    val buildOrganization = "com.github.gseitz.lensed"
    val buildVersion = "0.5"
    val buildScalaVersion = "2.9.0-1"

    val buildSettings: Seq[Setting[_]] = Defaults.defaultSettings ++ Seq[Setting[_]](
      organization := buildOrganization,
      version      := buildVersion,
      scalaVersion := buildScalaVersion
    )
  }

  object Dependencies {
    def scalaz   = "org.scalaz" %% "scalaz-core" % "6.0.1"
    def scalac   = "org.scala-lang" % "scala-compiler"
    def scalalib = "org.scala-lang" % "scala-library"
  }

  import BuildSettings.buildSettings
  import Dependencies._

  lazy val root = Project(
    id = "lensed",
    base = file(".")
  ) aggregate(annotation, plugin, examples)

  lazy val annotation = Project(
    id = "annotation",
    base = file("annotation"),
    settings = buildSettings
  )

  lazy val plugin = Project(
    id = "plugin",
    base = file("plugin"),
    settings = buildSettings ++ Seq[Setting[_]](
      libraryDependencies += scalaz,
      libraryDependencies <++= scalaVersion { sv =>
        scalac % sv ::
        scalalib % sv ::
        Nil
      }
    )
  ) dependsOn (annotation)

  lazy val examples = Project(
    id = "examples",
    base = file("examples")
  ) aggregate(testCaseClasses, usage)

//  val pluginArtifact =

  lazy val testCaseClasses = Project(
    id = "testCaseClasses",
    base  = file("examples/simple"),
    settings = buildSettings ++ Seq[Setting[_]](
      libraryDependencies += scalaz,
      scalacOptions <+= (packagedArtifact in Compile in plugin in packageBin) map (art => "-Xplugin:%s" format art._2.getAbsolutePath),
      scalacOptions += "-Xplugin-require:lensed"
    )
  ) dependsOn (plugin)

  lazy val usage = Project(
    id = "usage",
    base = file("examples/usage"),
    settings = buildSettings ++ Seq[Setting[_]](
      libraryDependencies += scalaz
    )
  ) dependsOn (testCaseClasses)


}
