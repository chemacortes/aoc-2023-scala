package day11

import inputs.Input.loadFileSync
import locations.Directory.currentDir

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}") // 10173804

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}") // 634324905172

def loadInput(): String = loadFileSync(s"$currentDir/../input/day11.txt")

val space = '.'
val galaxy = '#'

case class Position(x: Int, y: Int):
    def distance(that: Position) =
      (x - that.x).abs + (y - that.y).abs // Manhattan distance

case class Universe(private val input: String, gapDistance: Long = 2):
    val lines = input.linesIterator.toSeq
    val galaxies =
      for
          (line, i) <- lines.zipWithIndex
          (c, j) <- line.zipWithIndex
          if c == galaxy
      yield Position(i, j)

    val rowGaps =
      lines.zipWithIndex
        .collect:
            case (row, i) if row.forall(_ == space) => i
    val colGaps =
      lines.transpose.zipWithIndex
        .collect:
            case (column, j) if column.forall(_ == space) => j

    def expandedDistance(a: Position, b: Position): Long =
        val xmin = a.x min b.x
        val xmax = a.x max b.x
        val ymin = a.y min b.y
        val ymax = a.y max b.y

        val numRowGaps: Long =
          rowGaps
            .count: x =>
                xmin < x && x < xmax

        val numColGaps: Long =
          colGaps
            .count: y =>
                ymin < y && y < ymax

        (a distance b) + (numRowGaps + numColGaps) * (gapDistance - 1)

def run(gapDistance: Long, input: String) =
    val universe = Universe(input, gapDistance )
    universe.galaxies
      .combinations(2)
      .map:
          case Seq(a, b) =>
            universe.expandedDistance(a, b)
      .sum

def part1(input: String): String =
  run(gapDistance = 2, input).toString

end part1

def part2(input: String): String =
  run(gapDistance = 1000000L, input).toString
end part2
