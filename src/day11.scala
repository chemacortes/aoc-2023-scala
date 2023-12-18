package day11

import inputs.Input.loadFileSync
import locations.Directory.currentDir

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")  // 10173804

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day00.txt")

val space = '.'
val galaxy = '#'

case class Position(x: Int, y: Int):
    def distance(that: Position) =
      (x - that.x).abs + (y - that.y).abs // Manhattan distance

case class Universe(private val input: String):
    val lines = expandUniverse()
    val galaxies =
      for
          (line, i) <- lines.zipWithIndex
          (c, j) <- line.zipWithIndex
          if c == galaxy
      yield Position(i, j)

    def expandUniverse() =
        val lines =
          input.linesIterator
            .flatMap: line =>
                if line.forall(_ == space) then Seq(line, line)
                else Seq(line)
            .toSeq

        lines.transpose
          .flatMap: line =>
              if line.forall(_ == space) then Seq(line, line)
              else Seq(line)
          .transpose
          .map(_.mkString)

@main
def testing =
    val input = loadInput()
    val universe = Universe(input)
    universe.lines
      .foreach(println)

def part1(input: String): String =
    val universe = Universe(input)
    universe.galaxies
      .combinations(2)
      .map:
          case Seq(a, b) =>
            a distance b
      .sum
      .toString

end part1

def part2(input: String): String =
  ???
end part2
