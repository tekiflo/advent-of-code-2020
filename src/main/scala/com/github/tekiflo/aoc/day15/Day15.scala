package com.github.tekiflo.aoc.day15

import scala.annotation.tailrec

object Day15 {
  def result(input: Seq[Int], toIndex: Int): Int = {
    @tailrec def rec(nb: Int, occ: Map[Int, Int], i: Int): Int =
      if (i == toIndex - 1) nb
      else rec(occ.get(nb).fold(0)(i - _), occ.updated(nb, i), i + 1)
    rec(0, Map.from(input.zipWithIndex), input.size)
  }

  def resultPart1(input: Seq[Int]): Int = result(input, toIndex = 2020)
  def resultPart2(input: Seq[Int]): Int = result(input, toIndex = 30000000)

  def parseInput(raw: String): Seq[Int] = raw.split(',').toSeq.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val raw   = "16,12,1,0,15,7,11"
    val input = parseInput(raw)
    val part1 = resultPart1(input)
    println(s"day 15 part 1: $part1")
    val part2 = resultPart2(input)
    println(s"day 15 part 2: $part2")
  }
}
