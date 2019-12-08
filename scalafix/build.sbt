lazy val V = _root_.scalafix.sbt.BuildInfo

Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.ScalaLibrary

inThisBuild(
  List(
    organization := "com.mrdziuban",
    homepage := Some(url("https://github.com/mrdziuban/disable-tostring")),
    scmInfo := Some(ScmInfo(url("https://github.com/mrdziuban/disable-tostring"), "git@github.com:mrdziuban/disable-tostring.git")),
    developers := List(Developer("mrdziuban", "Matt Dziuban", "mrdziuban@gmail.com", url("https://github.com/mrdziuban"))),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    scalaVersion := V.scala212,
    addCompilerPlugin(scalafixSemanticdb),
    scalacOptions ++= List(
      "-Yrangepos",
      "-P:semanticdb:synthetics:on"
    ),
    scalacOptions --= Seq(
      "-language:existentials",
      "-language:experimental.macros",
      "-language:implicitConversions"
    )
  )
)

skip in publish := true

lazy val libs = Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.28",
  "org.typelevel" %% "cats-core" % "2.0.0"
)

lazy val rules = project.settings(
  moduleName := "scalafix",
  libraryDependencies ++= Seq(
    "ch.epfl.scala" %% "scalafix-core" % V.scalafixVersion,
    "ch.epfl.scala" %% "scalafix-rules" % V.scalafixVersion
  ) ++ libs
)

lazy val input = project.settings(
  skip in publish := true,
  libraryDependencies ++= libs
)

lazy val output = project.settings(
  skip in publish := true,
  libraryDependencies ++= libs
)

lazy val tests = project
  .settings(
    skip in publish := true,
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.scalafixVersion % Test cross CrossVersion.full,
    compile.in(Compile) :=
      compile.in(Compile).dependsOn(compile.in(input, Compile)).value,
    scalafixTestkitOutputSourceDirectories :=
      sourceDirectories.in(output, Compile).value,
    scalafixTestkitInputSourceDirectories :=
      sourceDirectories.in(input, Compile).value,
    scalafixTestkitInputClasspath :=
      fullClasspath.in(input, Compile).value,
  )
  .dependsOn(rules)
  .enablePlugins(ScalafixTestkitPlugin)
