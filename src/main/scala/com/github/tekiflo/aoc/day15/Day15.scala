package com.github.tekiflo.aoc.day15

object Day15 {
  def result(input: Seq[Int], toIndex: Int): Int = {
    val occ = input.zipWithIndex.foldLeft(Map.empty[Int, Int]) {
      case (occ, (nb, i)) => occ.updated(nb, i + 1)
    }
    val (res, _, _) = LazyList
      .iterate((0, occ, input.size + 1)) {
        case (nb, occ, i) => (occ.get(nb).fold(0)(i - _), occ.updated(nb, i), i + 1)
      }
      .dropWhile { case (_, _, i) => i != toIndex }
      .head
    res
  }

  def resultPart1(input: Seq[Int]): Int = result(input, toIndex = 2020)
  def resultPart2(input: Seq[Int]): Int = result(input, toIndex = 30000000)

  def parseInput(raw: String): Seq[Int] = raw.split(',').toSeq.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val raw   = "16,12,1,0,15,7,11"
    val input = parseInput(raw)
    val part1 = resultPart1(input)
    println(s"day 15 part 1: $part1")
    val part2 = resultPart2(input)
    println(s"day 15 part 2: $part2")
  }
}
