package day13

import inputs.Input.loadFileSync
import locations.Directory.currentDir

import scala.collection.immutable.ArraySeq

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}") // 33520

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}") // 34824

def loadInput(): String = loadFileSync(s"$currentDir/../input/day13.txt")

trait Rules:
    val maxDistance: Int

case class Grid(grid: String)(using rules: Rules):
    val rows: Seq[String] = grid.linesIterator.toSeq
    lazy val columns: Seq[String] = rows.transpose.map(_.mkString)

    def findHorSimetry =
      (rows zip rows.tail).zipWithIndex
        .collect:
            case ((r1, r2), i) if diffs(r1, r2) <= rules.maxDistance =>
              i + 1
        .filter(n => hasSimetry(rows, n))
        .headOption
        .map(_ * 100)

    def findVerSimetry =
      (columns zip columns.tail).zipWithIndex
        .collect:
            case ((c1, c2), i) if diffs(c1, c2) <= rules.maxDistance =>
              i + 1
        .filter(n => hasSimetry(columns, n))
        .headOption

    def diffs(line1: String, line2: String): Int =
      (line1 zip line2).count(_ != _)

    def hasSimetry(lines: Seq[String], n: Int): Boolean =
        val a = lines.take(n).reverse
        val b = lines.drop(n)

        (a zip b).map(diffs).sum == rules.maxDistance

object Grid:
    def parseGrids(input: String)(using rules: Rules): Seq[Grid] =
      ArraySeq.unsafeWrapArray(input.split("\n\n")).map(Grid.apply)

def part1(input: String): String =

    given Rules with
        val maxDistance = 0

    Grid
      .parseGrids(input)
      .map: grid =>
          (grid.findHorSimetry orElse grid.findVerSimetry).get
      .sum
      .toString

end part1

def part2(input: String): String =
    given Rules with
        val maxDistance = 1

    Grid
      .parseGrids(input)
      .map: grid =>
          (grid.findHorSimetry orElse grid.findVerSimetry).get
      .sum
      .toString

end part2
