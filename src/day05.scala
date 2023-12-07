package day05

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}") // 107430936

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day05")

case class Range(start: Long, range: Long):
    val last = start + range - 1

    // Used in part1
    def contains(n: Long) = start <= n && n <= last
    def map(dest: Long, n: Long) = n - start + dest

    // Used in part2
    def isEmpty = range == 0
    def map(dest: Long, r: Range): (List[Range], List[Range]) = // Used in part2
      ???

case class AlmanacMap(name: String, mapping: List[(Long, Range)]):
    def map(n: Long) =
      mapping.find((_, m) => m contains n) match
          case Some((dest, m)) => m.map(dest, n)
          case None            => n

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
        AlmanacMap(name, mappings.toList)
    .toList

def part1(input: String): String =
    val input = loadInput()

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
  ???
end part2
