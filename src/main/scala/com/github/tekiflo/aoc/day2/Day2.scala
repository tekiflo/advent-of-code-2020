package com.github.tekiflo.aoc.day2

import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex

object Day2 {
  case class Password(min: Int, max: Int, char: Char, value: String) {
    def isValidPart1: Boolean = {
      val occurences = value.count(_ == char)
      occurences >= min && occurences <= max
    }

    def isValidPart2: Boolean = {
      def isCorrect(index: Int) = Try(value.charAt(index - 1)).toOption.contains(char)
      isCorrect(min) ^ isCorrect(max)
    }
  }

  object Password {
    val PasswordRegex: Regex = "(\\d+)-(\\d+) (\\w): (\\w+)".r
    def apply(line: String): Password = line match {
      case PasswordRegex(min, max, char, value) => Password(min.toInt, max.toInt, char.head, value)
      case _                                    => throw new IllegalArgumentException(s"'$line' is not a valid line")
    }
  }

  def parseInput(input: Seq[String]): Seq[Password] =
    input.flatMap(l => Try(Password(l)).toOption)

  def howManyAreValidPart1(passwords: Seq[Password]): Int =
    passwords.count(_.isValidPart1)

  def howManyAreValidPart2(passwords: Seq[Password]): Int =
    passwords.count(_.isValidPart2)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day2/input.txt").getLines().toSeq
    val input = parseInput(lines)

    val resultPart1 = howManyAreValidPart1(input)
    println(s"$resultPart1 valid passwords for part 1")

    val resultPart2 = howManyAreValidPart2(input)
    println(s"$resultPart2 valid passwords for part 2")
  }
}
