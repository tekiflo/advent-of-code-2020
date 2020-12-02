package com.github.tekiflo.aoc.day2

import com.github.tekiflo.aoc.day2.Day2.{parseInput, Password}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day2Spec extends AnyFlatSpec with Matchers {
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

    Day2.parseInput(lines) must contain theSameElementsAs expectedResult
  }

  it should "return the correct result for part 1 given the test input" in {
    val expectedResult = 2

    Day2.howManyAreValid(parseInput(lines)) mustBe expectedResult
  }
}
