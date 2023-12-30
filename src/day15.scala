package day15

import inputs.Input.loadFileSync
import locations.Directory.currentDir

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day15.txt")

def hash(str: String): Int =
  str
    .map(_.toInt)
    .foldLeft(0): (acc, c) =>
        (acc + c) * 17 % 256

def part1(input: String): String =
  input.linesIterator
    .map: line =>
        line.split(",").map(hash).sum
    .next
    .toString

end part1

def part2(input: String): String =
  ???
end part2
