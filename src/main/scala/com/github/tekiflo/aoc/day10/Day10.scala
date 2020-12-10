package com.github.tekiflo.aoc.day10

import scala.io.Source

object Day10 {
  case class Chargers(all: List[Int]) {
    val deviceJoltage: Int = all.max + 3
    val sorted: List[Int]  = 0 :: (all.sorted :+ deviceJoltage)
    val diffs: List[Int]   = sorted.drop(1).lazyZip(sorted).map(_ - _)

    def resultPart1(): Int = diffs.count(_ == 1) * diffs.count(_ == 3)

    def resultPart2(): Long =
      diffs
        .foldLeft(0 :: Nil) {
          case (v :: rest, 1) => v + 1 :: rest
          case (acc, _)       => 0 :: acc
        }
        .filter(_ >= 2)
        .map {
          case 2 => 2L
          case 3 => 4L
          case _ => 7L
        }
        .product
  }

  def parseInput(lines: List[String]): Chargers = Chargers(lines.map(_.toInt))

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day10/input.txt").getLines().toList
    val input = parseInput(lines)
    println(s"day 10 part 1: ${input.resultPart1()}")
    println(s"day 10 part 2: ${input.resultPart2()}")
  }
}
