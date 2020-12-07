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

  val lines: Seq[String] = input.split('\n').toSeq

  val groupsUnion: Seq[Set[Char]]        = Day6.parseInputUnion(lines)
  val groupsIntersection: Seq[Set[Char]] = Day6.parseInputIntersection(lines)

  it should "correctly parse the input for part 1" in {
    val expected = Seq("abc", "abc", "abc", "a", "b").map(_.toSet)
    groupsUnion must contain theSameElementsAs expected
  }

  it should "return the correct result for part 1 given the test input" in {
    Day6.sumOfGroups(groupsUnion) mustBe 11
  }

  it should "correctly parse the input for part 2" in {
    val expected = Seq("abc", "", "a", "a", "b").map(_.toSet)
    groupsIntersection must contain theSameElementsAs expected
  }

  it should "return the correct result for part 2 given the test input" in {
    Day6.sumOfGroups(groupsIntersection) mustBe 6
  }
}
