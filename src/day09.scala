package day09

import inputs.Input.loadFileSync
import locations.Directory.currentDir

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day09.txt")

// Domain

trait Rules:
    def isFinal(history:History): Boolean

case class History(seq: Seq[Int])(using rules: Rules):
    lazy val isFinal = rules.isFinal(this)
    lazy val diffSequence =
      History:
          seq
            .sliding(2)
            .collect:
                case Seq(a, b) => (b - a)
            .toSeq
    
    def nextItem: Int =
      if isFinal then seq.last
      else
        seq.last + diffSequence.nextItem

object History:
    def parse(line: String)(using rules:Rules) =
      History:
          line
            .split(" ")
            .map(_.toInt)

def part1(input: String): String =

  given Rules with
    def isFinal(history:History) = 
      val seq = history.seq
      seq.forall(_==seq.head)

  val histories = 
    loadInput().linesIterator
      .map: line=>
        History.parse(line)

  histories
    .map: history =>
      history.nextItem
    .sum
    .toString

end part1

def part2(input: String): String =
  ???
end part2
