ThisBuild / organization  := "xyz.hyperreal"
ThisBuild / version       := "0.1.0-snapshot.1"
ThisBuild / scalaVersion  := "2.13.5"
ThisBuild / resolvers     += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation", "-feature", "-unchecked",
  "-language:postfixOps", "-language:implicitConversions", "-language:existentials", "-language:dynamics",
  "-Xasync")
ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
ThisBuild / libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"

lazy val root = (project in file("."))
  .dependsOn(bvm)
  .settings(
    name := "funl",
    libraryDependencies += "xyz.hyperreal" %% "indentation-lexical" % "0.9"
  )

lazy val bvm = project
  .dependsOn(dal)
  .settings(
    name := "bvm",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  )

lazy val dal = project
  .settings(
    name := "dal",
    libraryDependencies += "xyz.hyperreal" %% "numbers" % "0.1.0"
  )

lazy val matrix = project
  .dependsOn(dal)
  .settings(
    name := "matrix",
    libraryDependencies += "xyz.hyperreal" %% "table" % "1.0.0-snapshot.2"
  )

lazy val prolog = project
  .dependsOn(recursive_descent_parser)
  .dependsOn(bvm)
  .settings(
    name := "prolog",
    libraryDependencies += "jline" % "jline" % "2.14.6",
    libraryDependencies += "xyz.hyperreal" %% "args" % "0.2"
  )

lazy val recursive_descent_parser = project
  .settings(
    name := "recursive-descent-parser",
    libraryDependencies += "xyz.hyperreal" %% "pattern-matcher" % "0.3.15",
    libraryDependencies += "xyz.hyperreal" %% "pretty" % "0.2" % "test"
  )
