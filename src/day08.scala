package day08

import inputs.Input.loadFileSync
import locations.Directory.currentDir

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day08.txt")

type Tag = String
type Dirs = LazyList[Char]

final case class Node(left: Tag, right: Tag)

def dirs(directions:String): LazyList[Char] =
  LazyList.continually(directions.view).flatten

def parseMap(input: String): (Dirs, Map[Tag, Node]) =
    val lines = input.linesIterator

    val directions = lines.next
    lines.next
    val nodes =
      lines
        .map: line =>
            val s"$tag = ($left, $right)" = line: @unchecked
            (tag -> Node(left, right))

    (dirs(directions), nodes.toMap)

def followDir(
    nodes: Map[Tag, Node]
)(tag: Tag, directions: Dirs, steps: Long): Long =
  if tag == "ZZZ" then steps
  else
      directions.head match
          case 'L' =>
            followDir(nodes)(nodes(tag).left, directions.tail, steps + 1)
          case 'R' =>
            followDir(nodes)(nodes(tag).right, directions.tail, steps + 1)
          case _ =>
            throw new Exception("Exception thrown from func")

def part1(input: String): String =
  val (directions,nodes) = parseMap(input)

  followDir(nodes)("AAA",directions,0).toString

end part1

def part2(input: String): String =
  ???
end part2
