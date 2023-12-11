package day07

import inputs.Input.loadFileSync
import locations.Directory.currentDir

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}") // 249390788

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}") // 248750248

def loadInput(): String = loadFileSync(s"$currentDir/../input/day07.txt")

enum HandType(val value: Int):
    case FiveOfAKind extends HandType(10)
    case FourOfAKind extends HandType(9)
    case FullHouse extends HandType(8)
    case ThreeOfAKind extends HandType(7)
    case TwoPair extends HandType(6)
    case OnePair extends HandType(5)
    case HighCard extends HandType(0)

    def compare(that: HandType) = value compare that.value

// Part1
final case class Hand(cards: String, bid: Long):
    def handType: HandType =
        val counts =
          cards.distinct
            .map: c =>
                cards.count(_ == c)

        if counts contains 5 then HandType.FiveOfAKind
        else if counts contains 4 then HandType.FourOfAKind
        else if counts.contains(3) && counts.contains(2) then HandType.FullHouse
        else if counts contains 3 then HandType.ThreeOfAKind
        else if counts.count(_ == 2) == 2 then HandType.TwoPair
        else if counts contains 2 then HandType.OnePair
        else HandType.HighCard

implicit object HandOrdering extends Ordering[Hand]:

    private def compareHighCard(a: Hand, b: Hand): Int =
        val chars = "AKQJT98765432".reverse
        val (x, y) = (a.cards zip b.cards).find(_ != _).get

        chars.indexOf(x) compare chars.indexOf(y)

    def compare(a: Hand, b: Hand): Int =
        val res = a.handType compare b.handType
        if res != 0 then res
        else compareHighCard(a, b)

def parseHands(input: String) =
  input.linesIterator
    .map: line =>
        val s"$hand $bid" = line: @unchecked
        Hand(hand, bid.toLong)

def part1(input: String): String =
  parseHands(input).toList.sorted.zipWithIndex
    .map: (h, i) =>
        h.bid * (i + 1)
    .sum
    .toString

end part1

// Part2
final case class Hand2(cards: String, bid: Long):
    def handType: HandType =
        val js = cards.count(_ == 'J')
        val counts =
          cards.distinct
            .map: c =>
                cards.count(x => x == c && x != 'J')
            .toList

        if counts.max + js == 5 then HandType.FiveOfAKind
        else if counts.max + js == 4 then HandType.FourOfAKind
        else if counts.contains(3) && counts.contains(2) then HandType.FullHouse
        else if counts.count(_ == 2) == 2 && js == 1 then HandType.FullHouse
        else if counts.max + js == 3 then HandType.ThreeOfAKind
        else if counts.count(_ == 2) == 2 then HandType.TwoPair
        else if (counts contains 2) || js == 1 then HandType.OnePair
        else HandType.HighCard

implicit object HandOrdering2 extends Ordering[Hand2]:

    private def compareHighCard(a: Hand2, b: Hand2): Int =
        val chars = "AKQT98765432J".reverse
        val (x, y) = (a.cards zip b.cards).find(_ != _).get

        chars.indexOf(x) compare chars.indexOf(y)

    def compare(a: Hand2, b: Hand2): Int =
        val res = a.handType compare b.handType
        if res != 0 then res
        else compareHighCard(a, b)

def parseHands2(input: String) =
  input.linesIterator
    .map: line =>
        val s"$hand $bid" = line: @unchecked
        Hand2(hand, bid.toLong)

def part2(input: String): String =
  parseHands2(input).toList.sorted.zipWithIndex
    .map: (h, i) =>
        h.bid * (i + 1)
    .sum
    .toString

end part2
