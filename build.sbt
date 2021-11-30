val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name                                            := "aoc",
    version                                         := "0.1.0-SNAPSHOT",
    scalaVersion                                    := scala3Version,
    libraryDependencies += "com.novocode"            % "junit-interface"          % "0.11"   % "test",
    libraryDependencies += "org.scalactic"          %% "scalactic"                % "3.2.10",
    libraryDependencies += "org.scalatest"          %% "scalatest"                % "3.2.10" % "test",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"
  )
