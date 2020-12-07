package com.github.tekiflo.aoc.day6

import scala.io.Source

object Day6 {
  def parseInput(input: Seq[String]): Seq[Set[Char]] = {
    val (last, rest) = input.foldLeft((Set.empty[Char], List.empty[Set[Char]])) {
      case ((curr, acc), line) if line.isBlank => (Set.empty, curr :: acc)
      case ((curr, acc), line)                 => (curr ++ line.toSet, acc)
    }
    last :: rest
  }

  def sumOfGroups(groups: Seq[Set[Char]]): Int = groups.map(_.size).sum

  def main(args: Array[String]): Unit = {
    val lines  = Source.fromResource("day6/input.txt").getLines().toSeq
    val groups = parseInput(lines)

    val resultPart1 = sumOfGroups(groups)
    println(s"the sum of groups that answered yes is $resultPart1")
  }
}
