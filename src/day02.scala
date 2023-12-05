package day02

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day02")

case class CubeSet(red: Int, green: Int, blue: Int)

val gameIDRegex = """^Game (?<id>\d+)""".r
val cubesRegex = """(\d+) (red|green|blue)""".r

def part1(input: String): String =

    // Possible cubes
    val maxCubes = CubeSet(red = 12, green = 13, blue = 14)

    def outLimits(color: String, num: Int): Boolean =
      color match
          case "red"   => num > maxCubes.red
          case "green" => num > maxCubes.green
          case "blue"  => num > maxCubes.blue

    val possibleGames =
      for
          line <- loadInput().linesIterator
          cubes = cubesRegex
            .findAllMatchIn(line)
            .map(x => x.group(2) -> x.group(1).toInt)
          if cubes.forall((color, num) => !outLimits(color, num))
      yield gameIDRegex.findFirstMatchIn(line).get.group("id").toInt

    possibleGames.sum.toString

end part1

def part2(input: String): String =
    val games =
      for
          line <- loadInput().linesIterator
      yield
          val cubes =
            cubesRegex
              .findAllMatchIn(line)
              .map(x => x.group(2) -> x.group(1).toInt)
              .toList
          val maxCubes = 
            cubes.groupBy(_._1).map: (_, colors) =>
              colors.map((_,num)=>num).max
          maxCubes.product

    s"${games.sum}"

end part2
