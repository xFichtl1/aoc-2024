package day2

import scala.io.Source

def getIntListsFromInput: List[Array[Int]] =
  Source.fromResource("day2.txt")
    .getLines()
    .map(s => s.split(' ').map(_.toInt))
    .foldLeft(List.empty)((list, arrays) => arrays :: list)

@main def part1(): Unit =
  println(s"The solution for Part 1 is ${part1(getIntListsFromInput)}")

@main def part2(): Unit =
  println(s"The solution is for Part 2 is ${part2(getIntListsFromInput)}")

def isSafePart1(array: Array[Int]): Boolean =
  val (isStrictlyIncreasing, isStrictlyDecreasing, isDifferenceSafe) = array
    .sliding(2)
    .map { case Array(a, b) => (a < b, a > b, (a - b).abs < 4) }
    .foldLeft((true, true, true)) {
      case ((inc, dec, safe), (isInc, isDec, isSafeDiff)) =>
        (inc && isInc, dec && isDec, safe && isSafeDiff)
    }

  (isStrictlyIncreasing || isStrictlyDecreasing) && isDifferenceSafe

def isSafePart2(array: Array[Int]): Boolean =
  if (isSafePart1(array)) {
    true
  }
  else {
    // try removing each level and checking if it becomes safe
    array.indices.exists { idx =>
      isSafePart1(array.patch(idx, Nil, 1))
    }
  }

def part1(input: List[Array[Int]]): Int =
  input.count(isSafePart1)

def part2(input: List[Array[Int]]): Int =
  input.count(isSafePart2)

def isSafePart2False(array: Array[Int]): Boolean =
  var errorHappenedIdx = -1;
  val (isStrictlyIncreasing, isStrictlyDecreasing, isDifferenceSafe, _) = array
    .sliding(2)
    .map { case Array(a, b) => (a < b, a > b, (a - b).abs < 4) }
    .foldLeft((true, true, true, 0)) {
      case ((inc, dec, safe, currentIdx), (isInc, isDec, isSafeDiff)) =>
        if (!((isInc || isDec) && isSafeDiff) && errorHappenedIdx == -1) {
          errorHappenedIdx = currentIdx
        }
        (inc && isInc, dec && isDec, safe && isSafeDiff, currentIdx + 1)
    }

  if (errorHappenedIdx > 0 && errorHappenedIdx < array.length - 1) {
    val foo = isSafePart1(array.patch(errorHappenedIdx, Nil, 1))
    val bar = isSafePart1(array.patch(errorHappenedIdx + 1, Nil, 1))
    foo || bar
  } else {
    true
  }