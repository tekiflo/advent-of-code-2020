package com.github.tekiflo.aoc.day08

import scala.annotation.tailrec
import scala.io.Source

object Day08 {
  case class Program(
    instructions: Seq[Instruction],
    history: Set[Int],
    pos: Int,
    data: Int
  ) {
    def current: Instruction = instructions(pos)

    val isInLoop: Boolean = history.contains(pos)

    val isTerminated: Boolean = pos >= instructions.size

    def executeCurrent(): Program = current.exec(this)

    def moveCursor(delta: Int): Program = copy(history = history + pos, pos = pos + delta)

    def next(): Program = moveCursor(1)

    def accumulator(delta: Int): Program = copy(data = data + delta)

    def mutate(index: Int, instruction: Instruction): Program = copy(
      instructions = instructions.patch(index, Seq(instruction), 1)
    )
  }

  sealed trait Instruction {
    def value: Int
    def exec(ctx: Program): Program
    def switchType(): Option[Instruction]
  }
  object Instruction {
    final case class Noop(value: Int) extends Instruction {
      override def exec(ctx: Program): Program       = ctx.next()
      override def switchType(): Option[Instruction] = Some(Jump(value))
    }
    final case class Jump(value: Int) extends Instruction {
      override def exec(ctx: Program): Program       = ctx.moveCursor(value)
      override def switchType(): Option[Instruction] = Some(Noop(value))
    }
    final case class Acc(value: Int) extends Instruction {
      override def exec(ctx: Program): Program       = ctx.accumulator(value).next()
      override def switchType(): Option[Instruction] = None
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

  @tailrec def run(program: Program): Program =
    if (program.isInLoop || program.isTerminated) program
    else run(program.executeCurrent())

  def mutateAll(program: Program): Seq[Program] =
    program.instructions.zipWithIndex.flatMap {
      case (ins, index) =>
        ins.switchType().map(program.mutate(index, _))
    }

  def findMutatedEndingProgramData(program: Program): Int =
    mutateAll(program).map(run).find(_.isTerminated).fold(-1)(_.data)

  def main(args: Array[String]): Unit = {
    val lines   = Source.fromResource("day08/input.txt").getLines().toSeq
    val program = parseInput(lines)

    val resultPart1 = getDataBeforeLooping(program)
    println(s"data was $resultPart1 when program entered in an infinite loop")

    val resultPart2 = findMutatedEndingProgramData(program)
    println(s"data was $resultPart2 when mutated program terminated")
  }
}
