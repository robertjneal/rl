val scala3 = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "rl",
    version := "0.1.0",

    scalaVersion := scala3,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
    libraryDependencies += "org.scala-lang.modules" % "scala-parallel-collections_2.13" % "0.2.0"
  )
