package com.github.tekiflo.aoc.day3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day3Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 3"

  val lines = Seq(
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"
  )

  val road: Day3.Road = Day3.Road.parseInput(lines)

  it should "return the correct result for part 1 given the test input" in {
    val expectedResult = 7

    Day3.countTreesOnPath(road, Day3.part1Delta) mustBe expectedResult
  }

  it should "return the correct result for part 2 given the test input" in {
    val expectedResult = 336

    Day3.multiplyTreesOnAllPaths(road, Day3.part2Deltas) mustBe expectedResult
  }
}
