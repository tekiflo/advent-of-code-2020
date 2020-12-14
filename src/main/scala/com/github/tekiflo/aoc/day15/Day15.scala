package com.github.tekiflo.aoc.day15

import scala.annotation.tailrec

object Day15 {
  def resultPart1(input: Seq[Int], toIndex: Int = 2020): Int = {
    @tailrec def rec(last: Int, occ: Map[Int, Int], i: Int): Int =
      if (i == toIndex) last
      else rec(occ.get(last).fold(0)(i - _), occ.updated(last, i), i + 1)
    val (cur, occ, i) = input.foldLeft((-1, Map.empty[Int, Int], 0)) {
      case ((_, occ, i), nb) => (nb, occ.updated(nb, i), i + 1)
    }
    rec(cur, occ, i)
  }


  def parseInput(raw: String): Seq[Int] = raw.map(_.toInt).toSeq

  def main(args: Array[String]): Unit = {
    val raw   = "16,12,1,0,15,7,11"
    val input = parseInput(raw)
    val part1 = resultPart1(input)
    println(s"day 15 part 1: $part1")
  }
}
