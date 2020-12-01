package com.github.tekiflo.aoc.day1

import scala.io.Source
import scala.util.Try

object Day1 {
  def default(n: Int): Seq[Int] = Seq.fill(n)(-1)

  def findNSumResults(input: Seq[Int], toFind: Int, n: Int): Seq[Seq[Int]] = {
    def rec(index: Int, curr: List[Int], depth: Int): Seq[Seq[Int]] = depth match {
      case 0 => Seq.empty
      case 1 =>
        val currSum   = curr.sum
        val remaining = input.splitAt(index)._2
        remaining.find(currSum + _ == toFind).map(_ :: curr).toSeq
      case _ =>
        val numbers = input.zipWithIndex.splitAt(index)._2
        numbers.flatMap { case (nb, i) => rec(i + 1, nb :: curr, depth - 1) }
    }
    rec(index = 0, curr = Nil, depth = n)
  }

  def findNSumResult(input: Seq[Int], toFind: Int, n: Int): Seq[Int] =
    findNSumResults(input, toFind, n: Int).headOption.getOrElse(default(n))

  def resultFor(n: Int): Unit = {
    val toFind = 2020
    val input = Source
      .fromResource("day1/input.txt")
      .getLines()
      .flatMap(l => Try(l.toInt).toOption)
      .toSeq

    val numbers = findNSumResult(input, toFind, n)
    println(s"${numbers.mkString(" + ")} = $toFind")
    val result = numbers.product
    println(s"${numbers.mkString(" * ")} = $result")
  }

  def main(args: Array[String]): Unit = {
    resultFor(n = 2)
    resultFor(n = 3)
  }
}
