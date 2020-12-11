package com.github.tekiflo.aoc.day01

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day01Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 1"

  val input  = Seq(1721L, 979L, 366L, 299L, 675L, 1456L)
  val toFind = 2020L

  it should "return the correct result for part 1 given the test input" in {
    val n              = 2
    val expectedResult = Seq(1721L, 299L)

    Day01.findNSumResult(input, toFind, n) must contain theSameElementsAs expectedResult
  }

  it should "return the correct result for part 2 given the test input" in {
    val n              = 3
    val expectedResult = Seq(979L, 366L, 675L)

    Day01.findNSumResult(input, toFind, n) must contain theSameElementsAs expectedResult
  }
}
