val scala3 = "3.1.1"

lazy val harness = RootProject(file("../harness"))

lazy val book = project
  .in(file("."))
  .settings(
    name := "rl-book",
    version := "0.1.0",

    scalaVersion := scala3,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  ).dependsOn(harness)
