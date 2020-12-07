package com.github.tekiflo.aoc.day4

import scala.io.Source

object Day4 {
  case class Passport(entries: Map[String, String]) {
    def hasAllFields: Boolean =
      Passport.fieldsToCheck.forall(entries.keySet.contains)

    def isValid: Boolean =
      hasAllFields && entries.forall { case (k, v) => Passport.validate(k, v) }

    def +(entry: (String, String)): Passport =
      copy(entries = entries + entry)
  }

  object Passport {
    // byr (Birth Year) - four digits; at least 1920 and at most 2002.
    // iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    // eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    // hgt (Height) - a number followed by either cm or in:
    // If cm, the number must be at least 150 and at most 193.
    // If in, the number must be at least 59 and at most 76.
    // hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    // ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    // pid (Passport ID) - a nine-digit number, including leading zeroes.
    // cid (Country ID) - ignored, missing or not.
    def validate(key: String, value: String): Boolean = {
      def isBetween(min: Int, max: Int)(raw: String) =
        raw.toIntOption.exists(nb => nb >= min && nb <= max)
      def checkHeight(raw: String) = {
        val pattern = "(\\d+)(cm|in)".r
        raw match {
          case pattern(size, "cm") => isBetween(150, 193)(size)
          case pattern(size, "in") => isBetween(59, 76)(size)
          case _                   => false
        }
      }
      def isColor(raw: String)          = raw.matches("#[0-9a-f]{6}")
      def isEyeColor(raw: String)       = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(raw)
      def isPassportNumber(raw: String) = raw.matches("\\d{9}")
      key match {
        case "byr" => isBetween(1920, 2002)(value)
        case "iyr" => isBetween(2010, 2020)(value)
        case "eyr" => isBetween(2020, 2030)(value)
        case "hgt" => checkHeight(value)
        case "hcl" => isColor(value)
        case "ecl" => isEyeColor(value)
        case "pid" => isPassportNumber(value)
        case "cid" => true
        case _     => false
      }
    }

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

  def countValidPassportsPart1(passports: Seq[Passport]): Int =
    passports.count(_.hasAllFields)

  def countValidPassportsPart2(passports: Seq[Passport]): Int =
    passports.count(_.isValid)

  def main(args: Array[String]): Unit = {
    val lines     = Source.fromResource("day4/input.txt").getLines().toSeq
    val passports = parseInput(lines)
    val count     = passports.size

    val resultPart1 = countValidPassportsPart1(passports)
    println(s"$resultPart1/$count valid passports for part 1")
    val resultPart2 = countValidPassportsPart2(passports)
    println(s"$resultPart2/$count valid passports for part 2")
  }
}
