package com.github.tekiflo.aoc.day16

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day16Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 16"

  it should "return the correct result for part 1 given the test input" in {
    val lines: String =
      """class: 1-3 or 5-7
        |row: 6-11 or 33-44
        |seat: 13-40 or 45-50
        |
        |your ticket:
        |7,1,14
        |
        |nearby tickets:
        |7,3,47
        |40,4,50
        |55,2,20
        |38,6,12""".stripMargin

    val input: Day16.Tickets = Day16.parseInput(lines.split('\n').toSeq)

    Day16.resultPart1(input) mustBe 71
  }

  it should "return the correct result for part 2 given the test input" in {
    val lines: String =
      """class: 0-1 or 4-19
        |row: 0-5 or 8-19
        |seat: 0-13 or 16-19
        |
        |your ticket:
        |11,12,13
        |
        |nearby tickets:
        |3,9,18
        |15,1,5
        |5,14,9""".stripMargin

    val input: Day16.Tickets = Day16.parseInput(lines.split('\n').toSeq)

    input.mineAsMap() mustBe Map(
      "class" -> 12,
      "row"   -> 11,
      "seat"  -> 13
    )
  }
}
