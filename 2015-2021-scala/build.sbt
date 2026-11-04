lazy val root = project
  .in(file("."))
  .settings(
    name                                            := "aoc",
    version                                         := "0.1.0-SNAPSHOT",
    scalaVersion                                    := "3.10.0",
    libraryDependencies += "org.scalatest"          %% "scalatest"                % "3.2.20" % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.5.0"
  )
