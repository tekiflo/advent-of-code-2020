package com.github.tekiflo.aoc.day15

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day15Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 15"

  it should "return the correct result for part 1 given the test input" in {
    val data = Seq(
      ("0,3,6", 436),
      ("1,3,2", 1),
      ("2,1,3", 10),
      ("1,2,3", 27),
      ("2,3,1", 78),
      ("3,2,1", 438),
      ("3,1,2", 1836)
    ).map { case (raw, expected) => (Day15.parseInput(raw), expected) }

    data.foreach { case (input, expected) => Day15.resultPart1(input) mustBe expected }
  }

  it should "return the correct result for part 2 given the test input" in {
    val data = Seq(
      ("0,3,6", 175594),
      ("1,3,2", 2578),
      ("2,1,3", 3544142),
      ("1,2,3", 261214),
      ("2,3,1", 6895259),
      ("3,2,1", 18),
      ("3,1,2", 362)
    ).map { case (raw, expected) => (Day15.parseInput(raw), expected) }

    data.foreach { case (input, expected) => Day15.resultPart2(input) mustBe expected }
  }
}
