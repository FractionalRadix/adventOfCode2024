ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "adventOfCode2024",
    idePackagePrefix := Some("com.cormontia.adventOfCode2024")
  )
libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "latest.integration" % Test
)
//libraryDependencies ++= Seq(
//  "org.scalatest" %% "scalatest" % "latest.integration" % Test
//)
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "latest.integration" % Test
)
