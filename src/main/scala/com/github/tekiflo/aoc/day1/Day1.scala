package com.github.tekiflo.aoc.day1

import scala.io.Source
import scala.util.Try

object Day1 {
  val default: (Int, Int) = (-1, -1)

  def findTwoSumResults(input: Seq[Int], toFind: Int): Seq[(Int, Int)] =
    input.zipWithIndex.flatMap {
      case (nb1, index) =>
        val remaining = input.splitAt(index)._2
        remaining.find(nb1 + _ == toFind).map(nb2 => (nb1, nb2))
    }

  def findTwoSumResult(input: Seq[Int], toFind: Int): (Int, Int) =
    findTwoSumResults(input, toFind).headOption.getOrElse(default)

  def resultFor(input: Seq[Int], toFind: Int): Int = {
    val (nb1, nb2) = findTwoSumResult(input, toFind)
    nb1 * nb2
  }

  def main(args: Array[String]): Unit = {
    val toFind = 2020
    val input = Source
      .fromResource("day1/input.txt")
      .getLines()
      .flatMap(l => Try(l.toInt).toOption)
      .toSeq

    val (nb1, nb2) = findTwoSumResult(input, toFind)
    println(s"$nb1 + $nb2 = $toFind")
    val result = nb1 * nb2
    println(s"$nb1 * $nb2 = $result")
  }
}
