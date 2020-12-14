package com.github.tekiflo.aoc.day14

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day14Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 14"

  it should "return the correct result for part 1 given the test input" in {
    val lines: String =
      """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
        |mem[8] = 11
        |mem[7] = 101
        |mem[8] = 0""".stripMargin

    val input: Seq[Day14.Instruction] = Day14.parseInput(lines.split('\n').toSeq)

    Day14.resultPart1(input) mustBe 165
  }

  it should "return the correct result for part 2 given the test input" in {
    val lines: String =
      """mask = 000000000000000000000000000000X1001X
        |mem[42] = 100
        |mask = 00000000000000000000000000000000X0XX
        |mem[26] = 1""".stripMargin

    val input: Seq[Day14.Instruction] = Day14.parseInput(lines.split('\n').toSeq)

    Day14.resultPart2(input) mustBe 208
  }
}
