package com.github.tekiflo.aoc.day05

import scala.io.Source

object Day05 {
  def toLetters(nb: Int)(trueValue: Char, falseValue: Char, length: Int): String = {
    val result = nb.toBinaryString.map(c => if (c == '1') trueValue else falseValue)
    result.reverse.padTo(length, falseValue).reverse
  }

  def fromLetters(letters: String, trueValue: Char): Int =
    Integer.parseInt(letters.map(c => if (c == trueValue) '1' else '0'), 2)

  class BoardingPass(val row: Int, val column: Int) {
    val seatId: Int              = (row * 8) + column
    override def hashCode(): Int = seatId
    override def toString: String =
      toLetters(row)('B', 'F', 7) + toLetters(column)('R', 'L', 3)
  }

  object BoardingPass {
    def apply(row: Int, column: Int): BoardingPass = new BoardingPass(row, column)
    def apply(raw: String): BoardingPass = {
      val (row, column) = raw.trim.splitAt(raw.length - 3)
      BoardingPass(fromLetters(row, 'B'), fromLetters(column, 'R'))
    }
  }

  def parseInput(input: Seq[String]): Seq[BoardingPass] =
    input.map(BoardingPass(_))

  def highestSeatId(passes: Seq[BoardingPass]): Int = passes.map(_.seatId).max

  def mySeatId(passes: Seq[BoardingPass]): Int = {
    val seats = passes.map(_.seatId).toSet
    (seats.min to seats.max).find(!seats.contains(_)).getOrElse(-1)
  }

  def main(args: Array[String]): Unit = {
    val lines  = Source.fromResource("day05/input.txt").getLines().toSeq
    val passes = parseInput(lines)

    val resultPart1 = highestSeatId(passes)
    println(s"the highest seat ID is $resultPart1")

    val resultPart2 = mySeatId(passes)
    println(s"my seat ID is $resultPart2")
  }
}
