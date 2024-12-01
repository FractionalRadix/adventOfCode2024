ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "adventOfCode2024",
    idePackagePrefix := Some("com.cormontia.adventOfCode2024")
  )
