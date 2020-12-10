package com.github.tekiflo.aoc.day10

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day10Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 10"

  val linesExample1: String =
    """16
      |10
      |15
      |5
      |1
      |11
      |7
      |19
      |6
      |12
      |4""".stripMargin

  val linesExample2: String =
    """28
      |33
      |18
      |42
      |31
      |14
      |46
      |20
      |48
      |47
      |24
      |23
      |49
      |45
      |19
      |38
      |39
      |11
      |1
      |32
      |25
      |35
      |8
      |17
      |7
      |9
      |4
      |2
      |34
      |10
      |3""".stripMargin

  val inputExample1: Day10.Chargers = Day10.parseInput(linesExample1.split('\n').toList)
  val inputExample2: Day10.Chargers = Day10.parseInput(linesExample2.split('\n').toList)

  it should "return the correct result for part 1 given the test input" in {
    inputExample1.resultPart1() mustBe 7 * 5
    inputExample2.resultPart1() mustBe 22 * 10
  }
}
