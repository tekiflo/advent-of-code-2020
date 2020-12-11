package com.github.tekiflo.aoc.day11

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day11Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 11"

  val lines: String =
    """L.LL.LL.LL
      |LLLLLLL.LL
      |L.L.L..L..
      |LLLL.LL.LL
      |L.LL.LL.LL
      |L.LLLLL.LL
      |..L.L.....
      |LLLLLLLLLL
      |L.LLLLLL.L
      |L.LLLLL.LL""".stripMargin

  val input: Day11.Layout = Day11.parseInput(lines.split('\n').toSeq)

  it should "return the correct result for part 1 given the test input" in {
    Day11.resultPart1(input) mustBe 37
  }

  it should "return the correct result for part 2 given the test input" in {
    Day11.resultPart2(input) mustBe 26
  }
}
