package com.github.tekiflo.aoc.day4

import scala.io.Source

object Day4 {
  case class Passport(entries: Map[String, String]) {
    def isValid: Boolean =
      Passport.fieldsToCheck.forall(entries.keySet.contains)

    def +(entry: (String, String)): Passport =
      copy(entries = entries + entry)
  }

  object Passport {
    val neededFields: Seq[String] = Seq(
      "byr", // (Birth Year)
      "iyr", // (Issue Year)
      "eyr", // (Expiration Year)
      "hgt", // (Height)
      "hcl", // (Hair Color)
      "ecl", // (Eye Color)
      "pid", // (Passport ID)
      "cid"  // (Country ID)
    )

    val ignoredFields: Seq[String] = Seq("cid")

    val fieldsToCheck: Seq[String] = neededFields.diff(ignoredFields)

    val empty: Passport = Passport(Map.empty)
  }

  def parseInput(lines: Seq[String]): Seq[Passport] = {
    val (last, passports) = lines.foldLeft((Passport.empty, List.empty[Passport])) {
      case ((passport, acc), line) if line.isBlank => (Passport.empty, passport :: acc)
      case ((passport, acc), line) =>
        val updated: Passport = line.split("\\s+").foldLeft(passport) {
          case (passport, entry) =>
            entry.split(':') match {
              case Array(key, value) => passport + (key -> value)
              case _                 => passport
            }
        }
        (updated, acc)
    }
    last :: passports
  }

  def countValidPassports(passports: Seq[Passport]): Int =
    passports.count(_.isValid)

  def main(args: Array[String]): Unit = {
    val lines     = Source.fromResource("day4/input.txt").getLines().toSeq
    val passports = parseInput(lines)

    val count  = passports.size
    val result = countValidPassports(passports)
    println(s"$result/$count valid passports")
  }
}
