ThisBuild / organization  := "xyz.hyperreal"
ThisBuild / version       := "0.1.0-snapshot.1"
ThisBuild / scalaVersion  := "2.13.4"
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
    libraryDependencies += "xyz.hyperreal" %% "numbers-jvm" % "0.7.4"
  )
