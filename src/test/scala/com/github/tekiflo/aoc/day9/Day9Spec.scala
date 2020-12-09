package com.github.tekiflo.aoc.day9

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day9Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 9"

  val input: String =
    """35
      |20
      |15
      |25
      |47
      |40
      |62
      |55
      |65
      |95
      |102
      |117
      |150
      |182
      |127
      |219
      |299
      |277
      |309
      |576""".stripMargin

  val depth: Int = 5

  val xmas: Day9.XMAS = Day9.parseInput(input.split('\n').toSeq, depth)

  it should "return the correct result for part 1 given the test input" in {
    xmas.firstNotMatching mustBe 127
  }
}
