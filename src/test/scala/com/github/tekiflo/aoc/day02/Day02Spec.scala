package com.github.tekiflo.aoc.day02

import com.github.tekiflo.aoc.day02.Day02.{parseInput, Password}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day02Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 2"

  val lines = Seq(
    "1-3 a: abcde",
    "1-3 b: cdefg",
    "2-9 c: ccccccccc"
  )

  it should "correctly parse the test input" in {
    val expectedResult = Seq(
      Password(1, 3, 'a', "abcde"),
      Password(1, 3, 'b', "cdefg"),
      Password(2, 9, 'c', "ccccccccc")
    )

    Day02.parseInput(lines) must contain theSameElementsAs expectedResult
  }

  it should "return the correct result for part 1 given the test input" in {
    val expectedResult = 2

    Day02.howManyAreValidPart1(parseInput(lines)) mustBe expectedResult
  }

  it should "return the correct result for part 2 given the test input" in {
    val expectedResult = 1

    Day02.howManyAreValidPart2(parseInput(lines)) mustBe expectedResult
  }
}
