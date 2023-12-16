package day10

import inputs.Input.loadFileSync
import locations.Directory.currentDir

import scala.annotation.tailrec

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day10.txt")

enum Direction(val v: Int, val h: Int):
    case North extends Direction(-1, 0)
    case South extends Direction(1, 0)
    case East extends Direction(0, 1)
    case West extends Direction(0, -1)
    case None extends Direction(0, 0)

    def oposite =
      Direction.values
        .find: dir =>
            (dir.v, dir.h) == (-v, -h)
        .getOrElse(None)

val Ground = Tile('.')
val Start = Tile ('S')
case class Tile(c: Char):
    val isStart = c == 'S'
    val connect: Seq[Direction] =
      c match
          case '|' => Seq(Direction.North, Direction.South)
          case '-' => Seq(Direction.West, Direction.East)
          case 'L' => Seq(Direction.North, Direction.East)
          case 'J' => Seq(Direction.North, Direction.West)
          case '7' => Seq(Direction.South, Direction.West)
          case 'F' => Seq(Direction.South, Direction.East)
          case _   => Seq()

case class Position(x: Int, y: Int):
    def go(dir: Direction): Position = Position(x + dir.v, y + dir.h)

case class Field(private val input: String):
    lazy val fieldLines = input.linesIterator.toList
    lazy val start: Position =
        val (x, y) =
          fieldLines.zipWithIndex
            .collect:
                case (line, n) if line.contains('S') => (n, line.indexOf('S'))
            .head
        Position(x, y)

    def tileAt(pos: Position) =
        if pos.x < 0 || pos.y < 0 then
          Ground
        else
          val c = fieldLines(pos.x)(pos.y)
          Tile(c)

    def firstStep: Seq[Direction] =
      Direction.values
        .map: d =>
            d -> tileAt(start.go(d))
        .collect:
            case (d, t) if t.connect.contains(d.oposite) => d
        .toSeq

    def nextStep(pos: Position): Seq[Position] =
        val dirs =
          if pos == start then firstStep
          else tileAt(pos).connect

        dirs.map(pos.go(_))

@tailrec
def loop(
    field: Field,
    positions: Seq[Position],
    visited: Set[Position],
    distance: Int = 0
): Int =
    val nextPositions =
      positions
        .flatMap: pos =>
            field.nextStep(pos)
        .filterNot(visited.contains(_))
    if nextPositions.isEmpty then distance
    else loop(field, nextPositions, visited ++ positions, distance + 1)

def part1(input: String): String =
    val field = Field(input)
    loop(field, Seq(field.start), Set.empty, 0).toString

end part1

def part2(input: String): String =
  ???
end part2
