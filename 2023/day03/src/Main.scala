import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set

@main def main() =
  val grid = Grid.empty
  for line <- Source.fromResource("input.txt").getLines() do
    grid.parseRow(line)
  println(grid.sum)

val digits = "[0-9]+".r
val gear = "[*]".r

case class Grid(numbers: ArrayBuffer[Number], gears: Set[Position]):
  private var row = 0

  def parseRow(input: String): Unit =
    row += 1
    for found <- digits.findAllMatchIn(input) do
      val pos = Position(row, found.start)
      numbers.append(Number(pos, found.matched))
    for found <- gear.findAllMatchIn(input) do
      gears.add(Position(row, found.start))

  def sum: Int =
    numbers
      .groupBy(number => number.nearTo(gears))
      .filter((key, nums) => key != None && nums.length == 2)
      .values
      .map(nums => nums.map(n => n.text.toInt).multiply)
      .sum

case object Grid:
  def empty = Grid(ArrayBuffer(), Set())

case class Number(pos: Position, text: String):
  def adjacent = pos.adjacent(text.length())
  def nearTo(gears: Set[Position]): Option[Position] =
    adjacent.find(gears.contains)

case class Position(row: Int, col: Int):
  def adjacent(size: Int): Seq[Position] =
    for
      i <- row - 1 to row + 1
      j <- col - 1 to col + size
    yield Position(i, j)

extension (arr: ArrayBuffer[Int])
  def multiply: Int =
    arr match
      case ArrayBuffer() => 0
      case s => s.fold(1)(_ * _)
