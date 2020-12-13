package com.github.tekiflo.aoc.day13

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Day13 {
  case class Schedule(arrival: Long, buses: Seq[(Int, Long)]) {
    def nextBuses(): Seq[(Long, Long)] =
      buses.map(_._2).map(id => (id, id - arrival % id))

    def nextBus(): (Long, Long) = nextBuses().minBy { case (_, in) => in }
  }

  def resultPart1(schedule: Schedule): Long =
    schedule.nextBus().pipe { case (id, in) => id * in }

  def resultPart2(schedule: Schedule): Long =
    schedule.buses.foldLeft((0L, 1L)) { case ((q, r), (i, id)) => crt(q, r, id - i % id, id) }._1

  def egcd(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) =
    if (b == 0) (1, 0, a)
    else {
      val (s, t, g) = egcd(b, a % b)
      (t, s - (a / b) * t, g)
    }

  def lcm(a: BigInt, b: BigInt): BigInt = {
    val r = LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
    b * a / r
  }

  def crt(r1: Long, q1: Long, r2: Long, q2: Long): (Long, Long) = {
    val q3        = lcm(q1, q2)
    val (t, _, g) = egcd(q1 + q2, q3)
    val r3        = (r1 * q2 + r2 * q1) * t / g % q3
    if (r3 < 0 != q3 < 0) ((r3 + q3).toLong, q3.toLong)
    else (r3.toLong, q3.toLong)
  }

  def parseInput(lines: Seq[String]): Schedule = {
    val arrival = lines.head.toLong
    val buses = lines.tail.head
      .split(',')
      .zipWithIndex
      .flatMap {
        case ("x", _) => None
        case (s, i)   => Some((i, s.toLong))
      }
      .toSeq
    Schedule(arrival, buses)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day13/input.txt").getLines().toSeq
    val input = parseInput(lines)
    val part1 = resultPart1(input)
    println(s"day 13 part 1: $part1")
    val part2 = resultPart2(input)
    println(s"day 13 part 2: $part2")
  }
}
