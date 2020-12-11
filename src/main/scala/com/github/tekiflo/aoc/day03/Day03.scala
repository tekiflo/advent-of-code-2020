package com.github.tekiflo.aoc.day03

import scala.annotation.tailrec
import scala.io.Source

object Day03 {
  sealed abstract class Terrain(val value: Char)
  object Terrain {
    final case object Empty extends Terrain('.')
    final case object Tree  extends Terrain('#')

    val default: Terrain        = Empty
    def all: Set[Terrain]       = Set(Empty, Tree)
    def apply(c: Char): Terrain = all.find(_.value == c).getOrElse(default)
  }

  case class Point(x: Int, y: Int)

  case class Road(lines: Array[Array[Terrain]], pos: Point) {
    def apply(): Terrain = lines(pos.y)(pos.x)
    def move(delta: Point): Option[Road] = {
      val ny = pos.y + delta.y
      if (ny >= lines.length) {
        None
      } else {
        val nx = (pos.x + delta.x) % lines(ny).length
        Some(copy(pos = Point(nx, ny)))
      }
    }
  }

  object Road {
    def parseInput(input: Seq[String]): Road =
      Road(lines = input.map(_.map(Terrain(_)).toArray).toArray, pos = Point(0, 0))
  }

  def countTreesOnPath(road: Road, delta: Point): Int = {
    @tailrec def rec(road: Road, acc: Int): Int = road.move(delta) match {
      case Some(newRoad) =>
        rec(newRoad, acc + (if (newRoad() == Terrain.Tree) 1 else 0))
      case None =>
        acc
    }
    rec(road, 0)
  }

  def multiplyTreesOnAllPaths(road: Road, deltas: Seq[Point]): Long =
    deltas.map(countTreesOnPath(road, _).toLong).product

  val part1Delta: Point = Point(3, 1)

  val part2Deltas: Seq[Point] = Seq(
    Point(1, 1),
    Point(3, 1),
    Point(5, 1),
    Point(7, 1),
    Point(1, 2)
  )

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day03/input.txt").getLines().toSeq
    val road  = Road.parseInput(lines)

    val resultPart1 = countTreesOnPath(road, part1Delta)
    println(s"$resultPart1 trees on path")

    val resultPart2 = multiplyTreesOnAllPaths(road, part2Deltas)
    println(s"product of trees on all paths: $resultPart2")
  }
}
