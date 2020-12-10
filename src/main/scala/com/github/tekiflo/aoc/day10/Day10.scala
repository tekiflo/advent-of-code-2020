package com.github.tekiflo.aoc.day10

import scala.io.Source

object Day10 {
  case class Chargers(all: List[Int]) {
    val deviceJoltage: Int = all.max + 3
    val sorted: List[Int]  = 0 :: (all.sorted :+ deviceJoltage)
    val diffs: List[Int]   = sorted.drop(1).lazyZip(sorted).map(_ - _)

    def resultPart1(): Int = diffs.count(_ == 1) * diffs.count(_ == 3)
  }

  def parseInput(lines: List[String]): Chargers = Chargers(lines.map(_.toInt))

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day10/input.txt").getLines().toList
    val input = parseInput(lines)
    println(s"day 10 part 1: ${input.resultPart1()}")
  }
}
