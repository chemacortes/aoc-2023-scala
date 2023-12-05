package day04

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day04")

def getNumbers(s: String) =
  """\d{1,2}""".r.findAllIn(s).map(_.toInt).toList

def part1(input: String): String =
  loadInput().linesIterator
    .map: line =>
        val (s"Card $id: $winning | $hand") = line: @unchecked
        val winNumbers = getNumbers(winning)
        val matches =
          getNumbers(hand)
            .filter(n => winNumbers.contains(n))
            .length
        if matches == 0 then 0
        else 1 << matches - 1
    .sum
    .toString

end part1

case  class ScratchCard(val wins: Int, var copies: Int = 1)

def part2(input: String): String =

    val cards =
      loadInput().linesIterator
        .map: line =>
            val (s"Card $id: $winning | $hand") = line: @unchecked
            val winNumbers = getNumbers(winning)
            val matches =
              getNumbers(hand)
                .filter(n => winNumbers.contains(n))
                .length
            ScratchCard(matches)
        .toList

    for (card, i) <- cards.zipWithIndex
    do
        for n <- 1 to card.wins
        do cards(i+n).copies += card.copies
      
    cards.map(_.copies).sum.toString

end part2
