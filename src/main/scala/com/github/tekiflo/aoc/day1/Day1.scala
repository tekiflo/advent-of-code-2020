package com.github.tekiflo.aoc.day1

import scala.io.Source
import scala.util.Try

object Day1 {
  def default(n: Int): Seq[Long] = Seq.fill(n)(-1L)

  def findNSumResults(input: Seq[Long], toFind: Long, n: Int): Seq[Seq[Long]] = {
    def rec(index: Int, curr: List[Long], depth: Int): Seq[Seq[Long]] = depth match {
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

  def findNSumResult(input: Seq[Long], toFind: Long, n: Int): Seq[Long] =
    findNSumResults(input, toFind, n: Int).headOption.getOrElse(default(n))

  def resultFor(n: Int): Unit = {
    val toFind = 2020L
    val input = Source
      .fromResource("day1/input.txt")
      .getLines()
      .flatMap(l => Try(l.toLong).toOption)
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
