package com.github.tekiflo.aoc.day1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 1"

  it should "return the correct result given the test input" in {
    val input          = Seq(1721, 979, 366, 299, 675, 1456)
    val toFind         = 2020
    val expectedResult = (1721, 299)

    Day1.findTwoSumResult(input, toFind) mustBe expectedResult
  }
}
