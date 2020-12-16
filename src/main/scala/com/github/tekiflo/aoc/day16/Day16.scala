package com.github.tekiflo.aoc.day16

import scala.annotation.tailrec
import scala.io.Source

object Day16 {
  case class Tickets(checks: Map[String, Set[Int]], nearby: List[Seq[Int]], mine: Seq[Int]) {
    def addRange(name: String, r: Set[Int]): Tickets = this.copy(checks = checks + (name -> r))
    def addNearby(ticket: Seq[Int]): Tickets         = this.copy(nearby = ticket :: nearby)

    def invalidValues(): Seq[Int] =
      nearby.flatten.filterNot(value => checks.values.exists(_.contains(value)))

    def removeInvalidNearby(): Tickets =
      copy(nearby = nearby.filter(values => values.forall(value => checks.values.exists(_.contains(value)))))

    def findIndexes(): Map[String, Int] = {
      val filtered = removeInvalidNearby().nearby
        .map(_.map(value => checks.filter { case (_, r) => r.contains(value) }.keySet))
        .reduce(_ zip _ map { case (l, r) => l.intersect(r) })
      @tailrec def toUnique(acc: Seq[Set[String]]): Seq[String] =
        if (acc.forall(_.size == 1)) acc.flatMap(_.headOption)
        else {
          val uniques = acc.filter(_.size == 1).flatten
          val removed = acc.map {
            case u if u.size == 1 => u
            case l                => l -- uniques
          }
          toUnique(removed)
        }
      toUnique(filtered).zipWithIndex.toMap
    }

    def mineAsMap(): Map[String, Int] =
      findIndexes().view.mapValues(i => mine(i)).toMap
  }
  object Tickets {
    val empty: Tickets = Tickets(Map.empty, Nil, Seq.empty)
  }

  def resultPart1(input: Tickets): Int = input.invalidValues().sum

  def resultPart2(input: Tickets): Long =
    input
      .mineAsMap()
      .filter { case (k, _) => k.startsWith("departure") }
      .values
      .map(_.toLong)
      .product

  def parseInput(raw: Seq[String]): Tickets =
    raw
      .foldLeft((0, Tickets.empty)) {
        case (state, "")                 => state
        case ((0, t), "your ticket:")    => (1, t)
        case ((1, t), "nearby tickets:") => (2, t)
        case ((0, t), s"$name: $aa-$ab or $ba-$bb") =>
          (0, t.addRange(name, ((aa.toInt to ab.toInt) ++ (ba.toInt to bb.toInt)).toSet))
        case ((1, t), str)  => (1, t.copy(mine = str.split(',').toSeq.map(_.toInt)))
        case ((2, t), str)  => (2, t.addNearby(str.split(',').toSeq.map(_.toInt)))
        case (state, other) => throw new IllegalStateException(s"$other ($state)")
      }
      ._2

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day16/input.txt").getLines().toSeq
    val input = parseInput(lines)
    val part1 = resultPart1(input)
    println(s"day 16 part 1: $part1")
    val part2 = resultPart2(input)
    println(s"day 16 part 2: $part2")
  }
}
