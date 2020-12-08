package com.github.tekiflo.aoc.day8

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day8Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 8"

  val input: String =
    """nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |jmp -4
      |acc +6""".stripMargin

  val program: Day8.Program = Day8.parseInput(input.split('\n').toSeq)

  it should "return the correct result for part 1 given the test input" in {
    Day8.getDataBeforeLooping(program) mustBe 5
  }
}
