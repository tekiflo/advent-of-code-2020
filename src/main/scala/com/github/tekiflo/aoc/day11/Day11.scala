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

  def countOccupied(l: Layout)(x: Int, y: Int): Int = {
    val res: Seq[Char] = for {
      dx <- x - 1 to x + 1
      dy <- y - 1 to y + 1 if !(dx == x && dy == y)
      v  <- l.get((dx, dy))
    } yield v
    res.count(_ == occupied)
  }

  def round(base: Layout): (Layout, Boolean) =
    base.foldLeft((base, false)) {
      case ((acc, changed), ((x, y), c)) =>
        val mustSwitch =
          (c == occupied && countOccupied(base)(x, y) >= 4) ||
            (c == empty && countOccupied(base)(x, y) == 0)
        if (mustSwitch) (acc.updated((x, y), switched(c)), true)
        else (acc, changed)
    }

  @tailrec def resultPart1(layout: Layout): Int = round(layout) match {
    case (next, changed) if changed => resultPart1(next)
    case (next, _)                  => next.values.count(_ == occupied)
  }

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
  }
}
