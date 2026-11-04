val scala3Version = "3.7.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent of Code 2025",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    scalacOptions ++= Seq("-experimental", "-deprecation"),

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
  )
