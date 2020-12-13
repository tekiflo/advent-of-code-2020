package com.github.tekiflo.aoc.day12

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day12Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 12"

  val lines: String =
    """F10
      |N3
      |F7
      |R90
      |F11""".stripMargin

  val input: Day12.Instructions = Day12.parseInput(lines.split('\n').toSeq)

  it should "return the correct result for part 1 given the test input" in {
    Day12.resultPart1(input) mustBe 25
  }

  it should "return the correct result for part 2 given the test input" in {
    Day12.resultPart2(input) mustBe 286
  }
}
