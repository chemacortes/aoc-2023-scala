package day05

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day05")

case class MapRange(dest: Long, source: Long, range: Long):
    def contains(n: Long) = source <= n && n < source + range
    def map(n: Long): Long = n - source + dest

case class AlmanacMap(name: String, ranges: List[MapRange]):
    def map(n: Long) =
      ranges.find(r => r contains n) match
          case Some(r) => r.map(n)
          case None    => n

def getSeeds(line: String) =
    val s"seeds: $seeds0" = line: @unchecked
    """\d+""".r.findAllIn(seeds0).map(_.toLong).toList

def getMappings(text: String) =
  """(?s)[-a-z]+ map:[\n 0-9]+""".r
    .findAllIn(text)
    .map: block =>
        val it = block.linesIterator
        val s"$name map:" = it.next(): @unchecked
        val maps =
          it
            .withFilter(_.length > 0)
            .map: line =>
                val s"$dest $source $range" = line: @unchecked
                MapRange(dest.toLong, source.toLong, range.toLong)
        AlmanacMap(name, maps.toList)
    .toList

val almanacMaps =
  """
  seed-to-soil
  soil-to-fertilizer
  fertilizer-to-water
  water-to-light
  light-to-temperature
  temperature-to-humidity
  humidity-to-location
  """

def part1(input: String): String =
    val input = loadInput()
    val seeds = getSeeds(input.split("\n")(0))
    val mappings = getMappings(input)

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
