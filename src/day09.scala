package day09

import inputs.Input.loadFileSync
import locations.Directory.currentDir

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}") // 2008960228

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")  // 1097

def loadInput(): String = loadFileSync(s"$currentDir/../input/day09.txt")

// Domain

trait Rules:
    def isFinal(history: History): Boolean =
        val seq = history.seq
        seq.forall(_ == seq.head)
    def nextItem(history: History): Int

case class History(seq: Seq[Int])(using rules: Rules):
    lazy val isFinal = rules.isFinal(this)
    lazy val diffSequence =
      History:
          seq
            .sliding(2)
            .collect:
                case Seq(a, b) => (b - a)
            .toSeq

    lazy val nextItem: Int = rules.nextItem(this)

object History:

    def parse(line: String)(using rules: Rules) =
      History:
          line
            .split(" ")
            .map(_.toInt)

    def loadHistories(input: String)(using rules: Rules) =
      input.linesIterator
        .map: line =>
            History.parse(line)

def part1(input: String): String =

    given Rules with
        def nextItem(history: History): Int =
          if history.isFinal then history.seq.last
          else history.seq.last + history.diffSequence.nextItem

    History
      .loadHistories(input)
      .map: history =>
          history.nextItem
      .sum
      .toString

end part1

def part2(input: String): String =
    given Rules with
        def nextItem(history: History): Int =
          if history.isFinal then history.seq.head
          else history.seq.head - history.diffSequence.nextItem

    History
      .loadHistories(input)
      .map: history =>
          history.nextItem
      .sum
      .toString

end part2
