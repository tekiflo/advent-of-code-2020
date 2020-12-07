package com.github.tekiflo.aoc.day7

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day7Spec extends AnyFlatSpec with Matchers {
  behavior of "Day 7"

  val input: String =
    """light red bags contain 1 bright white bag, 2 muted yellow bags.
      |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
      |bright white bags contain 1 shiny gold bag.
      |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
      |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
      |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
      |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
      |faded blue bags contain no other bags.
      |dotted black bags contain no other bags.""".stripMargin

  val bags: Day7.Bags = Day7.parseInput(input.split('\n').toSeq)

  it should "correctly parse the input" in {
    val expectedBagNames = Seq(
      "light red",
      "dark orange",
      "bright white",
      "muted yellow",
      "shiny gold",
      "dark olive",
      "vibrant plum",
      "faded blue",
      "dotted black"
    )
    bags.all.map(_.name) must contain theSameElementsAs expectedBagNames
  }

  it should "return the correct result for part 1 given the test input" in {
    bags.howManyCanContain(Day7.myBag) mustBe 4
  }

  it should "return the correct result for part 2 given the test input" in {
    bags.howManyBagsFor(Day7.myBag) mustBe 32

    val otherInput =
      """shiny gold bags contain 2 dark red bags.
        |dark red bags contain 2 dark orange bags.
        |dark orange bags contain 2 dark yellow bags.
        |dark yellow bags contain 2 dark green bags.
        |dark green bags contain 2 dark blue bags.
        |dark blue bags contain 2 dark violet bags.
        |dark violet bags contain no other bags.""".stripMargin
    val otherBags: Day7.Bags = Day7.parseInput(otherInput.split('\n').toSeq)
    otherBags.howManyBagsFor(Day7.myBag) mustBe 126
  }
}
