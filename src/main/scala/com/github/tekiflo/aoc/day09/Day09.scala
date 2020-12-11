package com.github.tekiflo.aoc.day09

import com.github.tekiflo.aoc.day01.Day01

import scala.annotation.tailrec
import scala.io.Source

object Day09 {
  case class XMAS(all: Seq[Long], depth: Int) {
    def firstNotMatching: Long =
      (depth until all.size)
        .find { index =>
          val previous = all.slice(index - depth, index)
          val current  = all(index)
          val results  = Day01.findNSumResults(previous, current, n = 2)
          results.isEmpty
        }
        .map(all(_))
        .getOrElse(-1)

    def summingTo(nb: Long): Seq[Long] = {
      @tailrec def rec(size: Int): Seq[Long] =
        if (size == all.size) {
          Seq.empty
        } else {
          val result = (0 until all.size - size)
            .map(index => all.slice(index, index + size))
            .find(_.sum == nb)
          result match {
            case Some(value) => value
            case None        => rec(size + 1)
          }
        }
      rec(size = 3)
    }

    def encryptionWeakness: Long = {
      val notMatching = firstNotMatching
      val result      = summingTo(notMatching)
      result.min + result.max
    }
  }

  def parseInput(lines: Seq[String], depth: Int): XMAS =
    XMAS(lines.map(_.toLong), depth)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day09/input.txt").getLines().toSeq
    val depth = 25
    val xmas  = parseInput(lines, depth)

    val resultPart1 = xmas.firstNotMatching
    println(s"$resultPart1 is the first not matching number of the list")

    val resultPart2 = xmas.encryptionWeakness
    println(s"$resultPart2 is the encryption weakness")
  }
}
