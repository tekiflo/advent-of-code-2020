ThisBuild / scalaVersion := "2.13.4"
ThisBuild / scalafmtOnCompile := true

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val aoc = (project in file(".")).settings(
  name := "advent-of-code-2020",
  libraryDependencies ++= Dependencies.all
)
