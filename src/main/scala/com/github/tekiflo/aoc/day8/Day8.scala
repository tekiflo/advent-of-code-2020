package com.github.tekiflo.aoc.day8

import scala.annotation.tailrec
import scala.io.Source

object Day8 {
  case class Program(
    instructions: Seq[Instruction],
    history: Set[Int],
    pos: Int,
    data: Int
  ) {
    val current: Instruction = instructions(pos)

    val isInLoop: Boolean = history.contains(pos)

    def executeCurrent(): Program = current.exec(this)

    def moveCursor(delta: Int): Program = copy(history = history + pos, pos = pos + delta)

    def next(): Program = moveCursor(1)

    def accumulator(delta: Int): Program = copy(data = data + delta)
  }

  sealed trait Instruction {
    def value: Int
    def exec(ctx: Program): Program
  }
  object Instruction {
    final case class Noop(value: Int) extends Instruction {
      override def exec(ctx: Program): Program = ctx.next()
    }
    final case class Jump(value: Int) extends Instruction {
      override def exec(ctx: Program): Program = ctx.moveCursor(value)
    }
    final case class Acc(value: Int) extends Instruction {
      override def exec(ctx: Program): Program = ctx.accumulator(value).next()
    }

    def apply(line: String): Instruction = {
      val Array(name, rawValue) = line.split("\\s")
      val value                 = rawValue.toInt
      name match {
        case "nop" => Noop(value)
        case "jmp" => Jump(value)
        case "acc" => Acc(value)
      }
    }
  }

  def parseInput(lines: Seq[String]): Program = {
    val instructions = lines.map(Instruction(_))
    Program(
      instructions = instructions,
      history = Set.empty,
      pos = 0,
      data = 0
    )
  }

  @tailrec def getDataBeforeLooping(program: Program): Int =
    if (program.isInLoop) program.data
    else getDataBeforeLooping(program.executeCurrent())

  def main(args: Array[String]): Unit = {
    val lines   = Source.fromResource("day8/input.txt").getLines().toSeq
    val program = parseInput(lines)

    val resultPart1 = getDataBeforeLooping(program)
    println(s"data was $resultPart1 when program entered in an infinite loop")
  }
}
