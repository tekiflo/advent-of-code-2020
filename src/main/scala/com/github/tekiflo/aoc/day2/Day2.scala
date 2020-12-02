package com.github.tekiflo.aoc.day2

import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex

object Day2 {
  case class Password(min: Int, max: Int, char: Char, value: String) {
    def isValid: Boolean = {
      val occurences = value.count(_ == char)
      occurences >= min && occurences <= max
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

  def howManyAreValid(passwords: Seq[Password]): Int =
    passwords.count(_.isValid)

  def main(args: Array[String]): Unit = {
    val lines  = Source.fromResource("day2/input.txt").getLines().toSeq
    val input  = parseInput(lines)
    val result = howManyAreValid(input)
    println(s"$result valid passwords")
  }
}
