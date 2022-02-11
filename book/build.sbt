val scala3 = "3.1.1"

lazy val harness = RootProject(file("../harness"))

lazy val book = project
  .in(file("."))
  .settings(
    name := "rl-book",
    version := "0.1.0",

    scalaVersion := scala3,

    resolvers += "jvm-repr" at "https://maven.imagej.net/content/repositories/public/",

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "io.github.cibotech" % "evilplot_2.13" % "0.8.1",
    libraryDependencies += "io.github.cibotech" % "evilplot-repl_2.13" % "0.8.1"
  ).dependsOn(harness)
