package day3

import scala.io.Source
import scala.util.matching.Regex

def getInput: String =
  Source.fromResource("day3.txt")
    .getLines()
    .reduce(_.appendedAll(_))

@main def part1(): Unit =
  println(s"The solution for Part 1 is ${part1(getInput)}")

@main def part2(): Unit =
  println(s"The solution is for Part 2 is ${part2(getInput)}")

def part1(input: String): Int =
  val pattern: Regex = "mul\\((\\d+),(\\d+)\\)".r
  pattern.findAllMatchIn(input)
    .foldLeft(0)((result, regMatch) => result + regMatch.group(1).toInt * regMatch.group(2).toInt)

def part2(input: String): Int =
  val pattern: Regex = "(?<mulStmt>mul\\((?<i1>\\d+),(?<i2>\\d+)\\))|(?<dontStmt>don't)|(?<doStmt>do\\(\\))".r
  pattern.findAllMatchIn(input)
    .foldLeft((0, true))((currIt, regMatch) => regMatch match
      case m if m.group("mulStmt") != null =>
        (if (currIt._2) currIt._1 + regMatch.group("i1").toInt * regMatch.group("i2").toInt else currIt._1, currIt._2)
      case m if m.group("dontStmt") != null =>
        (currIt._1, false)
      case m if m.group("doStmt") != null =>
        (currIt._1, true)
    )._1