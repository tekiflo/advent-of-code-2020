package com.github.tekiflo.aoc.day6

import scala.io.Source

object Day6 {
  def parseInput(input: Seq[String])(f: (Set[Char], Set[Char]) => Set[Char]): Seq[Set[Char]] = {
    val (last, rest, _) = input.foldLeft((Set.empty[Char], List.empty[Set[Char]], true)) {
      case ((curr, acc, _), line) if line.isBlank => (Set.empty, curr :: acc, true)
      case ((_, acc, newLine), line) if newLine   => (line.toSet, acc, false)
      case ((curr, acc, _), line)                 => (f(curr, line.toSet), acc, false)
    }
    last :: rest
  }

  def parseInputUnion(input: Seq[String]): Seq[Set[Char]] =
    parseInput(input)(_ ++ _)

  def parseInputIntersection(input: Seq[String]): Seq[Set[Char]] =
    parseInput(input)(_ intersect _)

  def sumOfGroups(groups: Seq[Set[Char]]): Int = groups.map(_.size).sum

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day6/input.txt").getLines().toSeq

    val groupsPart1 = parseInputUnion(lines)
    val resultPart1 = sumOfGroups(groupsPart1)
    println(s"the sum of groups that answered yes is $resultPart1")

    val groupsPart2 = parseInputIntersection(lines)
    val resultPart2 = sumOfGroups(groupsPart2)
    println(s"the sum of groups that answered yes to all questions is $resultPart2")
  }
}
