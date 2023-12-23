package day12b

import inputs.Input.loadFileSync
import locations.Directory.currentDir

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day12.txt")

case class Condition(pattern: String, groups: Seq[Int]):

    def allCombinations(pat: String): Seq[String] =
      pat match
          case ""  => Seq("")
          case "?" => Seq(".", "#")
          case s"?$tail" =>
            for
                p <- allCombinations(tail)
                c <- ".#"
            yield c +: p
          case _ =>
            for p <- allCombinations(pat.tail)
            yield pat.head +: p

    def possibleGroups =
      allCombinations(pattern)
        .map: pat =>
            """#+""".r
              .findAllIn(pat)
              .map(_.size)
              .toSeq
        .count(_ == groups)

object Condition:
    def parse(s: String) =
        val Array(pat, g) = s.split(" ")
        Condition(pat, g.split(",").map(_.toInt).toIndexedSeq)

def part1(input: String): String =
  input.linesIterator
    .map: line =>
        Condition.parse(line).possibleGroups
    .sum
    .toString

end part1

def part2(input: String): String =
  ???
end part2
