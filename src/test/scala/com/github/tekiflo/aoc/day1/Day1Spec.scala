package com.github.tekiflo.aoc.day1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 1"

  val input  = Seq(1721, 979, 366, 299, 675, 1456)
  val toFind = 2020

  it should "return the correct result for part 1 given the test input" in {
    val n              = 2
    val expectedResult = Seq(1721, 299)

    Day1.findNSumResult(input, toFind, n) must contain theSameElementsAs expectedResult
  }

  it should "return the correct result for part 2 given the test input" in {
    val n              = 3
    val expectedResult = Seq(979, 366, 675)

    Day1.findNSumResult(input, toFind, n) must contain theSameElementsAs expectedResult
  }
}
