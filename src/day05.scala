package day05

import inputs.Input.loadFileSync
import locations.Directory.currentDir

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}") // 107430936

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day05.txt")

final case class Range(start: Long, range: Long):
    lazy val last = start + range - 1

    // Used in part1
    def contains(n: Long) = start <= n && n <= last
    def map(dest: Long, n: Long) = n - start + dest

    // Used in part2
    override def toString = s"[$start..$last]/$range"
    def isEmpty = range == 0L
    def map(dest: Long, r: Range): (List[Range], List[Range]) = // Used in part2
        val r1 = Range(r.start, r.range min (0L max (start - r.start)))
        val r2 =
          Range(r1.last + 1, (r.last - r1.last) min (0L max (last - r1.last)))
        val r3 = Range(r2.last + 1, 0L max (r.last - r2.last))
        (
          List(r1, r3).filter(!_.isEmpty),
          List(r2.copy(start = r2.start - start + dest)).filter(!_.isEmpty)
        )

final case class Mapping(name: String, ranges: List[(Long, Range)]):
    def map(n: Long) =
      ranges.find((_, m) => m contains n) match
          case Some((dest, m)) => m.map(dest, n)
          case None            => n

    // Used in part2
    override def toString() =
      s"""$name: ${ranges.map(_.toString).mkString(",")}"""
    def map(sources: List[Range]): List[Range] =
        val (nomapped, mapped) =
          ranges
            .foldLeft((sources, List[Range]())): (acc, z) =>
                val (nomapped0, mapped0) = acc
                val (dest, r) = z

                val (xs, ys) =
                  nomapped0
                    .map: s =>
                        r.map(dest, s)
                    .unzip

                (xs.flatten, mapped0 ++ ys.flatten)

        nomapped ++ mapped

def getSeeds(line: String) =
    val s"seeds: $seeds0" = line: @unchecked
    """\d+""".r.findAllIn(seeds0).map(_.toLong).toList

def getMappings(text: String) =
  /* Almanac Maps:
      - seed-to-soil
      - soil-to-fertilizer
      - fertilizer-to-water
      - water-to-light
      - light-to-temperature
      - temperature-to-humidity
      - humidity-to-location
   */
  """(?s)[-a-z]+ map:[\n 0-9]+""".r
    .findAllIn(text)
    .map: block =>
        val it = block.linesIterator
        val s"$name map:" = it.next(): @unchecked
        val mappings =
          it
            .withFilter(_.length > 0) // wipe empty lines
            .map: line =>
                val s"$dest $source $range" = line: @unchecked
                (dest.toLong, Range(source.toLong, range.toLong))
        Mapping(name, mappings.toList)
    .toList

def part1(input: String): String =
    val Array(seedsLine, almanacMaps) = input.split("\n", 2)
    val seeds = getSeeds(seedsLine)
    val mappings = getMappings(almanacMaps)

    seeds
      .map: seed =>
          mappings
            .foldLeft(seed): (s, m) =>
                m.map(s)
      .min
      .toString

end part1

def part2(input: String): String =
    val Array(seedsLine, almanacMaps) = input.split("\n", 2)
    val seeds = getSeeds(seedsLine)
    val mappings = getMappings(almanacMaps)

    val seedsRanges =
      seeds
        .grouped(2)
        .collect:
            case List(s, r) => Range(s, r)
        .toList

    mappings
      .foldLeft(seedsRanges): (sources, mapping) =>
          mapping.map(sources)
      .map: r =>
          r.start
      .min
      .toString

end part2
