val dottyVersion = "0.20.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "rl",
    org := "com.robertjneal",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
