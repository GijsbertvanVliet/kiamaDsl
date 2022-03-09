ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

libraryDependencies ++= Seq(
 "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.5.0",
  "org.scalatest" %% "scalatest" % "3.1.1" % "test"
)

lazy val root = (project in file("."))
  .settings(
    name := "kiamaDsl"
  )
