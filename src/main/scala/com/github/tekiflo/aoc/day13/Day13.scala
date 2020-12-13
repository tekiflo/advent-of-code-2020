package com.github.tekiflo.aoc.day13

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Day13 {
  case class Schedule(arrival: Long, buses: Set[Long]) {
    def nextBuses(): Seq[(Long, Long)] =
      buses
        .map(id => (id, id - arrival % id))
        .toSeq

    def nextBus(): (Long, Long) = nextBuses().minBy { case (_, in) => in }
  }

  def resultPart1(schedule: Schedule): Long =
    schedule.nextBus().pipe { case (id, in) => id * in }

  def parseInput(lines: Seq[String]): Schedule = {
    val arrival = lines.head.toLong
    val buses   = lines.tail.head.split(',').filterNot(_ == "x").map(_.toLong).toSet
    Schedule(arrival, buses)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day13/input.txt").getLines().toSeq
    val input = parseInput(lines)
    val part1 = resultPart1(input)
    println(s"day 13 part 1: $part1")
  }
}
