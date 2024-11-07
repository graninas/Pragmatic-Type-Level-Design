val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "rsc2p1p4_empty_adts",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
