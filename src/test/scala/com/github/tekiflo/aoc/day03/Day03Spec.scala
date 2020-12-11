package com.github.tekiflo.aoc.day03

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day03Spec extends AnyFlatSpec with Matchers {
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

  val road: Day03.Road = Day03.Road.parseInput(lines)

  it should "return the correct result for part 1 given the test input" in {
    val expectedResult = 7

    Day03.countTreesOnPath(road, Day03.part1Delta) mustBe expectedResult
  }

  it should "return the correct result for part 2 given the test input" in {
    val expectedResult = 336

    Day03.multiplyTreesOnAllPaths(road, Day03.part2Deltas) mustBe expectedResult
  }
}
