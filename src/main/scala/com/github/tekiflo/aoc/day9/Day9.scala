package com.github.tekiflo.aoc.day9

import com.github.tekiflo.aoc.day1.Day1

import scala.io.Source

object Day9 {
  case class XMAS(all: Seq[Long], depth: Int) {
    def firstNotMatching: Long =
      (depth until all.size)
        .find { index =>
          val previous = all.slice(index - depth, index)
          val current  = all(index)
          val results  = Day1.findNSumResults(previous, current, n = 2)
          results.isEmpty
        }
        .map(all(_))
        .getOrElse(-1)
  }

  def parseInput(lines: Seq[String], depth: Int): XMAS =
    XMAS(lines.map(_.toLong), depth)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day9/input.txt").getLines().toSeq
    val depth = 25
    val xmas  = parseInput(lines, depth)

    val resultPart1 = xmas.firstNotMatching
    println(s"$resultPart1 is the first not matching number of the list")
  }
}
