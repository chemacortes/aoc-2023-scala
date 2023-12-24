package day13

import inputs.Input.loadFileSync
import locations.Directory.currentDir

import scala.collection.immutable.ArraySeq

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}") // 33520

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day13.txt")

case class Grid(grid: String):
    val rows: Seq[String] = grid.linesIterator.toSeq
    lazy val columns: Seq[String] = rows.transpose.map(_.mkString)

    def findHorSimetry =
      (rows zip rows.tail).zipWithIndex
        .collect:
            case ((r1, r2), i) if r1 == r2 => i + 1
        .filter(n => hasSimetry(rows, n))
        .headOption
        .map(_ * 100)

    def findVerSimetry =
      (columns zip columns.tail).zipWithIndex
        .collect:
            case ((c1, c2), i) if c1 == c2 => i + 1
        .filter(n => hasSimetry(columns, n))
        .headOption

    def hasSimetry(lines: Seq[String], n: Int): Boolean =
        val a = lines.take(n).reverse
        val b = lines.drop(n)

        (a zip b).forall(_ == _)

object Grid:
    def parseGrids(input: String): Seq[Grid] =
      ArraySeq.unsafeWrapArray(input.split("\n\n")).map(Grid.apply)

def part1(input: String): String =
    val grids = Grid.parseGrids(input)

    grids
      .map: grid =>
          (grid.findHorSimetry orElse grid.findVerSimetry).get
      .sum
      .toString

end part1

def part2(input: String): String =
  ???
end part2
