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
    def turn(right: Boolean, from: Direction, degrees: Int): Direction = {
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
          case ((facing, acc), (Left(c), degrees))  => (turn(c == 'R', facing, degrees), acc)
        }
        ._2
        .reverse

    def endCoords(): (Int, Int) =
      compute().foldLeft((0, 0)) { case ((x, y), (d, nb)) => move(x, y, d, nb) }

    def move(x: Int, y: Int, d: Direction, nb: Int): (Int, Int) = d match {
      case Direction.East  => (x + nb, y)
      case Direction.West  => (x - nb, y)
      case Direction.North => (x, y + nb)
      case Direction.South => (x, y - nb)
    }

    def rotate(x: Int, y: Int, right: Boolean, degrees: Int): (Int, Int) = {
      val values    = Seq(x, y, -x, -y)
      val direction = if (right) 1 else -1
      def res(offset: Int) = {
        val res   = (offset + direction * (degrees / 90)) % values.length
        val index = if (res < 0) res + values.length else res
        values(index)
      }
      (res(0), res(1))
    }

    def withWaypoint(): (Int, Int) =
      all
        .map { case (d, v) => (Direction(d).toRight(d), v) }
        .foldLeft(((10, 1), (0, 0))) {
          case (((wx, wy), (x, y)), (Right(d), length)) => (move(wx, wy, d, length), (x, y))
          case (((wx, wy), (x, y)), (Left('F'), times)) => ((wx, wy), (x + times * wx, y + times * wy))
          case (((wx, wy), (x, y)), (Left(c), degrees)) => (rotate(wx, wy, c == 'R', degrees), (x, y))
        }
        ._2
  }

  def manhattan(coords: (Int, Int)): Int = {
    val (x, y) = coords
    Math.abs(x) + Math.abs(y)
  }

  def resultPart1(instructions: Instructions): Int = manhattan(instructions.endCoords())
  def resultPart2(instructions: Instructions): Int = manhattan(instructions.withWaypoint())

  def parseInput(lines: Seq[String]): Instructions = Instructions(
    lines.map(str => (str.head, str.tail.toInt))
  )

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day12/input.txt").getLines().toSeq
    val input = parseInput(lines)
    val part1 = resultPart1(input)
    println(s"day 12 part 1: $part1")
    val part2 = resultPart2(input)
    println(s"day 12 part 2: $part2")
  }
}
