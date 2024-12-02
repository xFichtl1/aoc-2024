import scala.io.Source

def getIntListsFromInput: (List[Int], List[Int]) =
  Source.fromResource("day1.txt")
    .getLines()
    .map(s => s.split(" {3}"))
    .collect {
      case Array(lVal, rVal) => (lVal.toInt, rVal.toInt)
    }
    .foldLeft(List.empty[Int], List.empty[Int])((lists, values) => (values._1 :: lists._1, values._2 :: lists._2))

@main def part1(): Unit =
  val (leftColList, rightColList) = getIntListsFromInput
  println(s"The solution for Part 1 is ${part1(leftColList, rightColList)}")

@main def part2(): Unit =
  val (leftColList, rightColList) = getIntListsFromInput
  println(s"The solution is for Part 2 is ${part2(leftColList, rightColList)}")

def part1(leftColList: List[Int], rightColList: List[Int]): Int =
  val (sortedLeftCol, sortedRightCol) = (leftColList.sorted, rightColList.sorted)

  sortedLeftCol.zip(sortedRightCol).map((lVal, rVal) => (lVal - rVal).abs).sum

def part2(leftColList: List[Int], rightColList: List[Int]): Int =
  val lookupCache = collection.mutable.Map[Int, Int]()

  leftColList.map(value => lookupCache.getOrElseUpdate(value, value * rightColList.count(value == _))).sum
