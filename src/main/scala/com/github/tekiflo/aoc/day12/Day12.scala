package com.github.tekiflo.aoc.day12

import scala.io.Source

object Day12 {
  sealed abstract class Direction(val c: Char)
  object Direction {
    final case object North extends Direction('N')
    final case object South extends Direction('S')
    final case object East  extends Direction('E')
    final case object West  extends Direction('W')
    val circle                            = Seq(North, East, South, West)
    def apply(c: Char): Option[Direction] = circle.find(_.c == c)
    def move(right: Boolean, from: Direction, degrees: Int): Direction = {
      val values = if (right) circle else circle.reverse
      values((values.indexOf(from) + (degrees / 90)) % values.length)
    }
  }
  case class Instructions(all: Seq[(Char, Int)]) {
    import Direction._
    def compute(): List[(Direction, Int)] =
      all
        .map { case (d, v) => (Direction(d).toRight(d), v) }
        .foldLeft((East: Direction, List.empty[(Direction, Int)])) {
          case ((facing, acc), (Right(d), length))  => (facing, (d -> length) :: acc)
          case ((facing, acc), (Left('F'), length)) => (facing, (facing -> length) :: acc)
          case ((facing, acc), (Left(c), degrees))  => (move(c == 'R', facing, degrees), acc)
        }
        ._2
        .reverse

    def endCoords(): (Int, Int) = {
      def move(x: Int, y: Int, d: Direction, nb: Int): (Int, Int) = d match {
        case Direction.East  => (x, y + nb)
        case Direction.West  => (x, y - nb)
        case Direction.North => (x + nb, y)
        case Direction.South => (x - nb, y)
      }
      compute().foldLeft((0, 0)) { case ((x, y), (d, nb)) => move(x, y, d, nb) }
    }
  }

  def resultPart1(instructions: Instructions): Int = {
    val (x, y) = instructions.endCoords()
    Math.abs(x) + Math.abs(y)
  }

  def parseInput(lines: Seq[String]): Instructions = Instructions(
    lines.map(str => (str.head, str.tail.toInt))
  )

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day12/input.txt").getLines().toSeq
    val input = parseInput(lines)
    val part1 = resultPart1(input)
    println(s"day 12 part 1: $part1")
  }
}
