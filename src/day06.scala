package day06

import inputs.Input.loadFileSync
import locations.Directory.currentDir

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}") // 170000

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}") // 20537782

def loadInput(): String = loadFileSync(s"$currentDir/../input/day06.txt")

def parseRecords(input: String): Seq[(Long, Long)] =
    val lines = input.linesIterator

    val Seq(times, distances) =
      lines
        .map: line =>
            """\d+""".r
              .findAllIn(line)
              .map(_.toLong)
              .toSeq
        .take(2)
        .toSeq

    times zip distances

def parseRecords2(input: String): (Long, Long) =
    val lines = input.linesIterator

    val Seq(time, distance) =
      lines
        .map: line =>
            """\d+.*\d+""".r
              .findFirstIn(line)
              .get
              .replaceAll(" ", "")
              .toLong
        .take(2)
        .toSeq

    (time, distance)

def calculation(time: Long, distance: Long) =
    val timeDouble = time.toDouble
    val record = distance + 1
    val z1 = (timeDouble - Math.sqrt(timeDouble * timeDouble - 4 * record)) / 2
    val z2 = (timeDouble + Math.sqrt(timeDouble * timeDouble - 4 * record)) / 2

    (z2.floor - z1.ceil + 1).toLong

def part1(input: String): String =
  parseRecords(input)
    .map(calculation)
    .product
    .toString

end part1

def part2(input: String): String =
    val (time, distance) = parseRecords2(input)

    calculation(time, distance).toString

end part2
