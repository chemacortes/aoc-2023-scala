package day14

import inputs.Input.loadFileSync
import locations.Directory.currentDir

import scala.annotation.tailrec

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}") // 109654

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}") // 94876

def loadInput(): String = loadFileSync(s"$currentDir/../input/day14.txt")

// Using opaque types to group all extension methods
object Grids:

    opaque type Grid = Seq[String]

    object Grid:
        def parse(input: String): Grid =
          input.linesIterator.toSeq.spin

    extension (grid: Grid)

        def spin: Grid =
          grid.transpose
            .map(_.mkString.reverse)

        def tilt: Grid =
          grid
            .map: line =>
                line
                  .split("#", -1)
                  .map(_.mkString.sorted)
                  .mkString("#")

        @tailrec
        def cycle(n: Int = 4): Grid =
          if n <= 0 then grid else grid.tilt.spin.cycle(n - 1)

        def load: Int =
          grid
            .map: line =>
                line.zipWithIndex
                  .collect:
                      case (c, i) if c == 'O' => i + 1
                  .sum
            .sum

def part1(input: String): String =
    import Grids.*

    val grid = Grid.parse(input)
    grid.tilt.load.toString
end part1

def part2(input: String): String =
    import Grids.*

    val CYCLES = 1_000_000_000

    val grid = Grid.parse(input)
    val states: LazyList[Grid] =
      LazyList.iterate(grid)(_.cycle())

    // Detecting same state --> cycle
    val (first, last) =
      states.zipWithIndex
        .map((state, n) => (states.take(n).indexOf(state), n))
        .find((a, _) => a >= 0)
        .get

    val loopsize = last - first
    val rest = (CYCLES - first) % loopsize
    val final_state = states(first + rest)

    final_state.load.toString

end part2
