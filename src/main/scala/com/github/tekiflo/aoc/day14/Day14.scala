package com.github.tekiflo.aoc.day14

import scala.io.Source

object Day14 {
  def bin2long(str: String, trueValue: Char = '1'): Long =
    java.lang.Long.parseLong(str.map(c => if (c == trueValue) '1' else '0'), 2)

  sealed trait Instruction
  object Instruction {
    case class Mask(pos: Long, neg: Long, floating: Long) extends Instruction {
      def part1(l: Long): Long = ~(~(l | pos) | neg)
      def part2(i: Long): Seq[Long] = {
        val ow = i | pos
        val fs = floating.toBinaryString
        val res = (0 until fs.length).foldLeft(Set(ow)) { (acc, index) =>
          val f = bin2long(fs.substring(index))
          acc.flatMap(n => Set(n | f, n & ~f))
        }
        res.toSeq
      }
    }
    object Mask {
      def apply(mask: String): Mask = Mask(
        pos = bin2long(mask, '1'),
        neg = bin2long(mask, '0'),
        floating = bin2long(mask, 'X')
      )
      val empty: Mask = Mask(0L, 0L, 0L)
    }
    case class Save(index: Long, value: Long) extends Instruction
    def apply(str: String): Instruction = str match {
      case s"mask = $mask"         => Mask(mask)
      case s"mem[$index] = $value" => Save(index.toLong, value.toLong)
    }
  }

  import Instruction._
  type Memory = Map[Long, Long]

  def run(instructions: Seq[Instruction])(saveF: (Mask, Save, Memory) => Memory): (Mask, Memory) =
    instructions.foldLeft((Mask.empty, Map.empty[Long, Long])) {
      case ((_, mem), newMask: Mask) => (newMask, mem)
      case ((mask, mem), save: Save) => (mask, saveF(mask, save, mem))
    }

  def memSummed(input: Seq[Instruction])(saveF: (Mask, Save, Memory) => Memory): Long =
    run(input)(saveF)._2.values.sum

  def resultPart1(input: Seq[Instruction]): Long = memSummed(input) {
    case (mask, Save(index, value), mem) => mem.updated(index, mask.part1(value))
  }

  def resultPart2(input: Seq[Instruction]): Long = memSummed(input) {
    case (mask, Save(index, value), mem) => mem ++ mask.part2(index).map(_ -> value).toMap
  }

  def parseInput(lines: Seq[String]): Seq[Instruction] = lines.map(Instruction(_))

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day14/input.txt").getLines().toSeq
    val input = parseInput(lines)
    val part1 = resultPart1(input)
    println(s"day 14 part 1: $part1")
    val part2 = resultPart2(input)
    println(s"day 14 part 2: $part2")
  }
}
