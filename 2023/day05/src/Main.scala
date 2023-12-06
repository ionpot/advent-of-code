import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set
import scala.util.boundary

@main def main() =
  val lines = Source.fromResource("input.txt").getLines()
  val alm = Almanac.empty
  alm.parse(lines)
  println(alm.lowest)

case class Almanac(seeds: Set[Long], maps: ArrayBuffer[Mapping]):
  def lowest: Long = seeds.map(locationOf).min
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
    input
      .split(": ")
      .last
      .split(" ")
      .map(s => s.toLong)
      .foreach(seeds.add)
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
