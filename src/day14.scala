package day14

import inputs.Input.loadFileSync
import locations.Directory.currentDir

import scala.annotation.tailrec

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}") // 109654

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}") // 94876

def loadInput(): String = loadFileSync(s"$currentDir/../input/day14.txt")

// Lighter version of Grid class
type GridT = Seq[String]

extension (grid: GridT)

    def spin: GridT =
      grid.transpose
        .map(_.mkString.reverse)

    def tilt: GridT =
      grid
        .map: line =>
            line
              .split("#", -1)
              .map(_.mkString.sorted)
              .mkString("#")

    @tailrec
    def cycle(n: Int = 4): GridT =
      if n <= 0 then grid else grid.tilt.spin.cycle(n - 1)

object Grid:
    def parse(input: String): Grid =
      Grid(input.linesIterator.toSeq.spin)

case class Grid(grid: GridT):

    override def toString(): String = grid.mkString("\n")
    def spin = Grid(grid.spin)
    def tilt = Grid(grid.tilt)
    def cycle(n: Int = 4) = Grid(grid.cycle(n))

    def load: Int =
      grid
        .map: line =>
            line.zipWithIndex
              .collect:
                  case (c, i) if c == 'O' => i + 1
              .sum
        .sum

def part1(input: String): String =
    val grid = Grid.parse(input)
    grid.tilt.load.toString
end part1

def part2(input: String): String =

    val CYCLES = 1_000_000_000

    val grid = Grid.parse(input)
    val states: LazyList[GridT] =
      LazyList.iterate(grid.grid)(_.cycle())

    // Detecting same state --> cycle
    val (first, last) =
      states.zipWithIndex
        .map((state, n) => (states.take(n).indexOf(state), n))
        .find((a, _) => a >= 0)
        .get

    val loopsize = last - first
    val rest = (CYCLES - first) % loopsize
    val final_state = states(first + rest)

    Grid(final_state).load.toString

end part2
