package com.github.tekiflo.aoc.day13

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day13Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 13"

  val lines: String =
    """939
      |7,13,x,x,59,x,31,19""".stripMargin

  val input: Day13.Schedule = Day13.parseInput(lines.split('\n').toSeq)

  it should "return the correct result for part 1 given the test input" in {
    Day13.resultPart1(input) mustBe 295L
  }
}
