package com.github.tekiflo.aoc.day3

import scala.annotation.tailrec
import scala.io.Source

object Day3 {
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

  def countTreesOnPath(road: Road): Int = {
    val delta = Point(3, 1)
    @tailrec def rec(road: Road, acc: Int): Int = road.move(delta) match {
      case Some(newRoad) =>
        rec(newRoad, acc + (if (newRoad() == Terrain.Tree) 1 else 0))
      case None =>
        acc
    }
    rec(road, 0)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day3/input.txt").getLines().toSeq
    val road  = Road.parseInput(lines)

    val result = countTreesOnPath(road)
    println(s"$result trees on path")
  }
}
