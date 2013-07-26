name := "fresh-scala"

version := "0.1.0"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "1.9.1",
	                    "com.googlecode.kiama" %% "kiama" % "1.5.1")

mainClass := None
