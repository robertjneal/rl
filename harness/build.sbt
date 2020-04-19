val dottyVersion = "0.20.0-RC1"
val monocleVersion = "2.0.0" // depends on cats 2.x

lazy val root = project
  .in(file("."))
  .settings(
    name := "rl",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.github.julien-truffaut" %  "monocle-core_2.13"  % monocleVersion,
    libraryDependencies += "com.github.julien-truffaut" %  "monocle-macro_2.13" % monocleVersion,
    libraryDependencies += "com.github.julien-truffaut" %  "monocle-law_2.13"   % monocleVersion % "test",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
    libraryDependencies += "org.scala-lang.modules" % "scala-parallel-collections_2.13" % "0.2.0"
  )
