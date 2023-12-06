import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set
import scala.util.boundary
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global

@main def main() =
  val lines = Source.fromResource("input.txt").getLines()
  val alm = Almanac.empty
  alm.parse(lines)
  for lowest <- alm.lowest
  do println(lowest)

case class Almanac(seedSet: Set[Seeds], maps: ArrayBuffer[Mapping]):
  def lowest: Future[Long] =
    val tasks = seedSet.map(seeds => Future(lowestOf(seeds)))
    for locations <- Future.sequence(tasks)
    yield locations.min
  def lowestOf(seeds: Seeds): Long =
    var min = 0L
    for seed <- seeds.first to seeds.last do
      val loc = locationOf(seed)
      if min == 0L || min > loc then min = loc
    min
  def locationOf(seed: Long): Long =
    var value = seed
    for mapping <- maps do value = mapping.map(value)
    value
  def parse(lines: Iterator[String]): Unit =
    val line = lines.next()
    if line.startsWith("seeds:") then addSeeds(line)
    else if line.endsWith("map:") then addMapping(lines)
    if lines.hasNext then parse(lines)
  def addSeeds(input: String): Unit =
    for seeds <- Seeds.parse(input)
    do seedSet.addOne(seeds)
  def addMapping(lines: Iterator[String]): Unit =
    val ranges = MapRange.parse(lines)
    maps.addOne(Mapping(ranges))

case object Almanac:
  def empty = Almanac(Set(), ArrayBuffer())

case class Mapping(ranges: Seq[MapRange]):
  def map(input: Long): Long =
    boundary:
      for range <- ranges do
        range.map(input) match
          case None => ()
          case Some(value) => boundary.break(value)
      input

case class MapRange(dest: Long, source: Long, length: Long):
  def map(input: Long): Option[Long] =
    val i = input - source
    if (0L <= i && i < length) Some(dest + i) else None

case object MapRange:
  def parse(
      lines: Iterator[String],
      ranges: Seq[MapRange] = Seq(),
  ): Seq[MapRange] =
    val line = lines.next()
    line.split(" ") match
      case Array(dest, source, length) =>
        val range = MapRange(dest.toLong, source.toLong, length.toLong)
        val appended = ranges.appended(range)
        if lines.hasNext then parse(lines, appended)
        else appended
      case _ => ranges

case class Seeds(first: Long, count: Long):
  def last: Long = first + count - 1

case object Seeds:
  def parse(input: String): Seq[Seeds] =
    val numbers = input
      .split(": ")
      .last
      .split(" ")
      .map(s => s.toLong)
    for
      i <- 0 until numbers.length - 1
      if i % 2 == 0
    yield Seeds(first = numbers(i), count = numbers(i + 1))
