package day14

import inputs.Input.loadFileSync
import locations.Directory.currentDir

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}") // 109654

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day14.txt")

case class Grid(input: Seq[String]):

    def tiltNorth: Grid =
      Grid(
        input.transpose
          .map(_.mkString)
          .map: line =>
              line
                .split("#", -1)
                .map(_.mkString.sorted.reverse)
                .mkString("#")
          .transpose
          .map(_.mkString)
      )

    def calculate: Long =
      input.transpose
        .map(_.mkString.reverse)
        .map(_.zipWithIndex.collect:
            case (c, i) if c == 'O' => i + 1
        )
        .map(_.sum.toLong)
        .sum

def part1(input: String): String =
    val grid = Grid(input.linesIterator.toSeq)
    grid.tiltNorth.calculate.toString

end part1

def part2(input: String): String =
  ???
end part2
