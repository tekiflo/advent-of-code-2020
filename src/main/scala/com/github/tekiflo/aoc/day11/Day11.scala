package com.github.tekiflo.aoc.day11

import scala.annotation.tailrec
import scala.io.Source

object Day11 {
  type Layout = Map[(Int, Int), Char]

  val floor    = '.'
  val empty    = 'L'
  val occupied = '#'

  def switched(c: Char): Char =
    if (c == occupied) empty
    else if (c == empty) occupied
    else c

  def occupiedPart1(l: Layout)(x: Int, y: Int): Int = {
    val res: Seq[Char] = for {
      dx <- x - 1 to x + 1
      dy <- y - 1 to y + 1 if !(dx == x && dy == y)
      v  <- l.get((dx, dy))
    } yield v
    res.count(_ == occupied)
  }

  def occupiedPart2(l: Layout)(x: Int, y: Int): Int = {
    @tailrec def rec(dx: Int, dy: Int, cx: Int, cy: Int): Boolean =
      l.get((cx, cy)) match {
        case Some(c) if c == occupied => true
        case Some(c) if c == empty    => false
        case Some(_)                  => rec(dx, dy, cx + dx, cy + dy)
        case None                     => false
      }
    def inSight(dx: Int, dy: Int): Boolean = rec(dx, dy, x + dx, y + dy)
    val res: Seq[Boolean] = for {
      dx <- -1 to 1
      dy <- -1 to 1 if !(dx == 0 && dy == 0)
    } yield inSight(dx, dy)
    res.count(b => b)
  }

  def round(countFunc: Layout => (Int, Int) => Int, threshold: Int)(base: Layout): (Layout, Boolean) =
    base.foldLeft((base, false)) {
      case ((acc, changed), ((x, y), c)) =>
        val mustSwitch =
          (c == occupied && countFunc(base)(x, y) >= threshold) ||
            (c == empty && countFunc(base)(x, y) == 0)
        if (mustSwitch) (acc.updated((x, y), switched(c)), true)
        else (acc, changed)
    }

  def result(roundF: Layout => (Layout, Boolean))(layout: Layout): Int = {
    @tailrec def rec(layout: Layout): Int = roundF(layout) match {
      case (next, changed) if changed => rec(next)
      case (next, _)                  => next.values.count(_ == occupied)
    }
    rec(layout)
  }

  def show(l: Layout): Unit = {
    val res = l
      .groupBy(_._1._1)
      .toSeq
      .sortBy(_._1)
      .map(_._2.toSeq.sortBy(_._1._2).map(_._2).mkString(""))
      .mkString("\n")
    println(res)
    println("\n")
  }

  def resultPart1(layout: Layout): Int = result(round(occupiedPart1, threshold = 4))(layout)
  def resultPart2(layout: Layout): Int = result(round(occupiedPart2, threshold = 5))(layout)

  def parseInput(lines: Seq[String]): Layout = {
    val res = for {
      (line, x) <- lines.zipWithIndex
      (c, y)    <- line.zipWithIndex
    } yield (x, y) -> c
    res.toMap
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day11/input.txt").getLines().toSeq
    val input = parseInput(lines)
    val part1 = resultPart1(input)
    println(s"day 11 part 1: $part1")
    val part2 = resultPart2(input)
    println(s"day 11 part 2: $part2")
  }
}
