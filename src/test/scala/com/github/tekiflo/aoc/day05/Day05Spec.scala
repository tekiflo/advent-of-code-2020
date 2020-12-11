package com.github.tekiflo.aoc.day05

import com.github.tekiflo.aoc.day05.Day05.BoardingPass
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day05Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 5"

  val testSet: Seq[(String, BoardingPass, Int)] = Seq(
    ("BFFFBBFRRR", BoardingPass(row = 70, column = 7), 567),
    ("FFFBBBFRRR", BoardingPass(row = 14, column = 7), 119),
    ("BBFFBBFRLL", BoardingPass(row = 102, column = 4), 820)
  )

  val input: Seq[String] = testSet.map { case (input, _, _) => input }

  it should "correctly parse the input" in {
    val parsed  = Day05.parseInput(input)
    val rowCols = testSet.map { case (_, pass, _) => pass }.map(p => (p.row, p.column))
    val seatIds = testSet.map { case (_, _, seatId) => seatId }

    parsed.map(_.toString) must contain theSameElementsAs input
    parsed.map(p => (p.row, p.column)) must contain theSameElementsAs rowCols
    parsed.map(_.seatId) must contain theSameElementsAs seatIds
  }

  it should "return the correct result for part 1 given the test input" in {
    val passes = Day05.parseInput(input)

    Day05.highestSeatId(passes) mustBe 820
  }
}
