ThisBuild / scalaVersion := "2.12.12"
ThisBuild / sbtVersion := "1.3.13"

lazy val root = (project in file("."))
  .aggregate(mazesolver.js, mazesolver.jvm)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val mazesolver = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    name := "Mazesolver",
    version := "0.1.0-SNAPSHOT",
    organization := "org.pfcoperez",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.3" % Test
  )
  .jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies ++= Seq(
      //"org.typelevel" %% "cats-core" % "2.1.1",
      "org.typelevel" %% "cats-collections-core" % "0.7.0"
    )
  )
  .jsSettings(
    // Add JS-specific settings here
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0",
    scalaJSUseMainModuleInitializer := true
  )
