package com.github.tekiflo.aoc.day08

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day08Spec extends AnyFlatSpec with Matchers {
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

  val program: Day08.Program = Day08.parseInput(input.split('\n').toSeq)

  it should "return the correct result for part 1 given the test input" in {
    Day08.getDataBeforeLooping(program) mustBe 5
  }

  it should "return the correct result for part 2 given the test input" in {
    Day08.findMutatedEndingProgramData(program) mustBe 8
  }
}
