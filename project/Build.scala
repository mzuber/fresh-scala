import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    version := "0.1",
    scalaVersion := "2.10.2",
    scalacOptions ++= Seq("-deprecation", "-feature"  /*, "-Ymacro-debug-lite" */)
  )
}

object Dependencies {
  val scalatest = "org.scalatest" % "scalatest_2.10" % "2.0.M5b"
  val kiama = "com.googlecode.kiama" %% "kiama" % "1.5.1"
}

object FreshScalaBuild extends Build {
  import BuildSettings._
  import Dependencies._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings
  ) aggregate(core, examples)

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings ++ Seq(
      name := "fresh-scala",
      mainClass := None,
      parallelExecution in Test := false,
      libraryDependencies += scalatest,
      libraryDependencies += kiama
    )
  )

  lazy val examples: Project = Project(
    "examples",
    file("examples"),
    settings = buildSettings
  ) dependsOn(core)
}
