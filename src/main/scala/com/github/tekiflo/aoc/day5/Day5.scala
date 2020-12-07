package com.github.tekiflo.aoc.day5

import scala.io.Source

object Day5 {
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

  def main(args: Array[String]): Unit = {
    val lines  = Source.fromResource("day5/input.txt").getLines().toSeq
    val passes = parseInput(lines)

    val result = highestSeatId(passes)
    println(s"the highest seat ID is $result")
  }
}
