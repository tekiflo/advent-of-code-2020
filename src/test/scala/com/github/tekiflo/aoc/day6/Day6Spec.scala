package com.github.tekiflo.aoc.day6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day6Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 6"

  val input: String =
    """abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b""".stripMargin

  val groups: Seq[Set[Char]] = Day6.parseInput(input.split('\n').toSeq)

  it should "correctly parse the input" in {
    val expected = Seq("abc", "abc", "abc", "a", "b").map(_.toSet)
    groups must contain theSameElementsAs expected
  }

  it should "return the correct result for part 1 given the test input" in {
    Day6.sumOfGroups(groups) mustBe 11
  }
}
