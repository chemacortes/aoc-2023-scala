package day03

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.util.matching.Regex.Match

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

//def loadInput(): String = loadFileSync(s"$currentDir/../input/day00")
def loadInput(): String = loadFileSync(s"$currentDir/../input/day03")

case class Coord(x: Int, y: Int):
    def withIn(start: Coord, end: Coord) =
      if y < start.y || y > end.y then false
      else if x < start.x || x > end.x then false
      else true

case class PartNumber(value: Int, start: Coord, end: Coord)
case class Symbol(sym: String, pos: Coord):
    def neighborOf(number: PartNumber) =
      pos.withIn(
        Coord(number.start.x - 1, number.start.y - 1),
        Coord(number.end.x + 1, number.end.y + 1)
      )

object IsInt:
    def unapply(in: Match): Option[Int] =
      in.matched.toIntOption

def findPartsAndSymbols(input: String) =
  input
    .split("\n")
    .zipWithIndex
    .flatMap: (line, i) =>
        """(\d+|[^.\d])""".r
          .findAllMatchIn(line)
          .map:
              case m @ IsInt(value) =>
                PartNumber(value, Coord(m.start, i), Coord(m.end - 1, i))
              case s => Symbol(s.matched, Coord(s.start, i))

def part1(input: String): String =

    val all = findPartsAndSymbols(loadInput())
    val symbols = all.collect { case s: Symbol => s }

    all
      .collect:
          case n: PartNumber if symbols.exists(_.neighborOf(n)) => n.value
      .sum
      .toString

end part1

def part2(input: String): String =
    val all = findPartsAndSymbols(loadInput())
    val numbers = all.collect { case n: PartNumber => n }
    val gears = all.collect { case s: Symbol if s.sym == "*" => s }

    gears
      .map: g =>
          val neighbors = numbers.filter(g.neighborOf(_))
          if neighbors.length == 2 then neighbors(0).value * neighbors(1).value
          else 0
      .sum
      .toString
end part2
