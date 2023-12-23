package day12

import inputs.Input.loadFileSync
import locations.Directory.currentDir

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}") // 7260

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}") // 1909291258644

def loadInput(): String = loadFileSync(s"$currentDir/../input/day12.txt")

type Pattern = String
type Memoize = Map[Condition, Long]

case class Condition(pattern: Pattern, groups: List[Int])
object Condition:
    def parse(str: String, fold: Int = 1): Condition =
        val Array(pat, g) = str.split(" ")
        val groups = g.split(",").map(_.toInt).toList
        if fold == 1 then Condition(pat, groups)
        else
            val patFolded = List.fill(fold)(pat).mkString("?")
            val groupsFolded = List.fill(fold)(groups).flatten
            Condition(patFolded, groupsFolded)

extension (pattern: Pattern)
    def startMatch(str: String) =
      (str.length <= pattern.length) &&
        (pattern zip str)
          .forall: (a, b) =>
              a == '?' || a == b

    def fullMatch(str: String) =
      pattern
        .zipAll(str, '?', '.')
        .forall: (a, b) =>
            a == '?' || a == b

def possibleArrangements(memo: Memoize)(cond: Condition): (Memoize, Long) =
  if memo.contains(cond) then (memo, memo(cond))
  else
      cond.groups match

          case Nil => (memo, 0L)

          case head :: Nil =>
            val group = "#" * head
            val n: Long =
              (0 to cond.pattern.length - head)
                .count: p =>
                    cond.pattern.fullMatch("." * p + group)

            (memo + (cond -> n), n)

          case head :: tail =>
            val firstGroup = "#" * head + "."
            val remain = tail.sum + tail.size // 1,2,3 -> .#.##.###
            (0 to cond.pattern.length - remain)
              .map(n => "." * n + firstGroup)
              .filter(group => cond.pattern.startMatch(group))
              .foldRight((memo, 0L)):
                  case (g, (memoAcc, n)) =>
                    val cond2 = Condition(cond.pattern.drop(g.length), tail)
                    val (memo2, m) = possibleArrangements(memoAcc)(cond2)
                    (memo2 + (cond2 -> m), n + m)

def part1(input: String): String =
    val conditions = input.linesIterator.map(Condition.parse(_))

    conditions
      .map(possibleArrangements(Map.empty)(_))
      .map(_._2)
      .sum
      .toString

end part1

def part2(input: String): String =
    val conditions = input.linesIterator.map(Condition.parse(_, fold = 5))

    conditions
      .map(possibleArrangements(Map.empty)(_))
      .map(_._2)
      .sum
      .toString

end part2
