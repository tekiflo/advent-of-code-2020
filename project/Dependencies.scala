import sbt._

object Dependencies {
  object Version {
    val scalaTest = "3.2.2"
  }

  private val testDependencies = Seq(
    "org.scalatest" %% "scalatest" % Version.scalaTest
  ).map(_ % Test)

  val all: Seq[ModuleID] = testDependencies
}
