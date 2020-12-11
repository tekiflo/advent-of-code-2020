package com.github.tekiflo.aoc.day07

import scala.io.Source

object Day07 {
  case class Bag(name: String, children: Map[String, Int]) {
    def canContain(bagName: String, all: Seq[Bag]): Boolean =
      name != bagName && children.keys.exists { key =>
        key == bagName || all.find(_.name == key).exists(_.canContain(bagName, all))
      }

    def howManyBags(all: Seq[Bag]): Int =
      if (children.isEmpty) 0
      else
        children.map {
          case (key, i) =>
            val nb = all.find(_.name == key).fold(0)(_.howManyBags(all))
            i + (nb * i)
        }.sum
  }

  object Bag {
    def apply(line: String): Bag = {
      val qtyBagPattern = "(\\d+) (\\w+ \\w+) bags?".r
      def children(list: String): Map[String, Int] =
        list
          .split(", ")
          .map {
            case qtyBagPattern(qty, name) => name -> qty.toInt
          }
          .toMap
      line match {
        case s"$name bags contain $list." if list == "no other bags" => Bag(name, Map.empty)
        case s"$name bags contain $list."                            => Bag(name, children(list))
      }
    }
  }

  case class Bags(all: Seq[Bag]) {
    def howManyCanContain(bagName: String): Int =
      all.count(_.canContain(bagName, all))

    def howManyBagsFor(bagName: String): Int =
      all.find(_.name == bagName).fold(-1)(_.howManyBags(all))
  }

  def parseInput(lines: Seq[String]): Bags = Bags(lines.map(Bag(_)))

  val myBag = "shiny gold"

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day07/input.txt").getLines().toSeq
    val bags  = parseInput(lines)
    val size  = bags.all.size

    val resultPart1 = bags.howManyCanContain(myBag)
    println(s"$resultPart1/$size bags can contain $myBag")

    val resultPart2 = bags.howManyBagsFor(myBag)
    println(s"$resultPart2 bags are needed for $myBag")
  }
}
