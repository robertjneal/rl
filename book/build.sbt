val scala3 = "3.0.0-M2"

lazy val harness = RootProject(file("../harness"))

lazy val book = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := scala3,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  ).dependsOn(harness)
