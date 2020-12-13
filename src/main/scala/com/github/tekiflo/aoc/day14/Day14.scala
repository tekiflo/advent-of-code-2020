package com.github.tekiflo.aoc.day14

import scala.io.Source

object Day14 {
  def bin2long(str: String, trueValue: Char): Long =
    java.lang.Long.parseLong(str.map(c => if (c == trueValue) '1' else '0'), 2)

  sealed trait Instruction
  object Instruction {
    case class Mask(pos: Long, neg: Long) extends Instruction {
      def apply(l: Long): Long = ~(~(l | pos) | neg)
    }
    case class Save(index: Int, value: Long) extends Instruction
    def apply(str: String): Instruction = str match {
      case s"mask = $mask"         => Mask(bin2long(mask, '1'), bin2long(mask, '0'))
      case s"mem[$index] = $value" => Save(index.toInt, value.toLong)
    }
  }

  import Instruction._
  type Memory = Map[Int, Long]
  def applyAll(instructions: Seq[Instruction]): (Mask, Memory) =
    instructions.foldLeft((Mask(0L, 0L), Map.empty[Int, Long])) {
      case ((_, mem), newMask: Mask)         => (newMask, mem)
      case ((mask, mem), Save(index, value)) => (mask, mem.updated(index, mask(value)))
    }

  def resultPart1(input: Seq[Instruction]): Long = {
    val (_, mem) = applyAll(input)
    mem.values.sum
  }

  def parseInput(lines: Seq[String]): Seq[Instruction] = lines.map(Instruction(_))

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day14/input.txt").getLines().toSeq
    val input = parseInput(lines)
    val part1 = resultPart1(input)
    println(s"day 14 part 1: $part1")
  }
}
