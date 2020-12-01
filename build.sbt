ThisBuild / scalaVersion := "2.13.4"
ThisBuild / scalafmtOnCompile := true

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / githubWorkflowDependencyPatterns += "project/*.scala"
ThisBuild / githubWorkflowJavaVersions := Seq("zulu@1.11")
ThisBuild / githubWorkflowSbtCommand := "csbt"
ThisBuild / githubWorkflowTargetBranches := Seq("main")

lazy val aoc = (project in file(".")).settings(
  name := "advent-of-code-2020",
  libraryDependencies ++= Dependencies.all
)
