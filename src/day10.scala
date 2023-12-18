package day10

import inputs.Input.loadFileSync
import locations.Directory.currentDir

import scala.annotation.tailrec

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}") // 6867

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}") // 595

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
val Start = Tile('S')
case class Tile(c: Char):
    val isStart = c == 'S'
    val connect: Set[Direction] =
      c match
          case '|' => Set(Direction.North, Direction.South)
          case '-' => Set(Direction.West, Direction.East)
          case 'L' => Set(Direction.North, Direction.East)
          case 'J' => Set(Direction.North, Direction.West)
          case '7' => Set(Direction.South, Direction.West)
          case 'F' => Set(Direction.South, Direction.East)
          case _   => Set()

case class Position(x: Int, y: Int):
    def go(dir: Direction): Position = Position(x + dir.v, y + dir.h)

case class Field(private val input: String):
    val fieldLines = input.linesIterator.toList
    val (height, width) = (fieldLines.length, fieldLines(0).length)
    lazy val start: Position =
        val (x, y) =
          fieldLines.zipWithIndex
            .collect:
                case (line, n) if line.contains('S') => (n, line.indexOf('S'))
            .head
        Position(x, y)

    def tileAt(pos: Position) =
      if pos.x < 0 || pos.y < 0 then Ground
      else
          val c = fieldLines(pos.x)(pos.y)
          Tile(c)

    def firstStep: Set[Direction] =
      Direction.values
        .map: d =>
            d -> tileAt(start.go(d))
        .collect:
            case (d, t) if t.connect.contains(d.oposite) => d
        .toSet

    def nextStep(pos: Position): Set[Position] =
        val dirs =
          if pos == start then firstStep
          else tileAt(pos).connect

        dirs.map(pos.go(_))

@tailrec
def loop(
    field: Field,
    positions: Set[Position],
    visited: Set[Position],
    distance: Int = 0
): (Set[Position], Int) =
    val nextPositions =
      positions
        .flatMap: pos =>
            field.nextStep(pos)
        .filterNot(visited.contains(_))
    if nextPositions.isEmpty then (visited ++ positions, distance)
    else loop(field, nextPositions, visited ++ positions, distance + 1)

def part1(input: String): String =
    val field = Field(input)
    loop(field, Set(field.start), Set.empty, 0)._2.toString

end part1

def part2(input: String): String =
    val field = Field(input)
    val visited = loop(field, Set(field.start), Set.empty)._1

    val NS = """[|]"""
    val NES="""L-*7"""
    val NWS = """F-*J"""
    val vertPat = s"""($NS|$NES|$NWS)""".r

    // TODO: buggy implementation: does not consider starting point as special case
    def isInside(pos: Position) =
        val line = field.fieldLines(pos.x).take(pos.y)
        val line2 =
          line.zipWithIndex
            .map: (c, n) =>
                if visited.contains(Position(pos.x, n)) then c else '.'
            .mkString

        vertPat.findAllIn(line2).size % 2 != 0

    (for
        x <- 0 until field.height
        y <- 0 until field.width
    yield
        val pos = Position(x, y)
        if !visited.contains(pos) && isInside(pos) then 1
        else 0
    ).sum.toString

end part2
