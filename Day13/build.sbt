ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "Day13"
  )
  .dependsOn(ProjectRef(uri("../Common"), "root"))

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.14.1"
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.14.1"
libraryDependencies += "com.fasterxml.jackson.module" % "jackson-module-scala_3" % "2.14.1"
