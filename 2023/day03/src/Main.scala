import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set

@main def main() =
  val grid = Grid.empty
  for line <- Source.fromResource("input.txt").getLines() do
    grid.parseRow(line)
  println(grid.sum)

val digit = "[0-9]+".r
val symbol = "[^.0-9]".r

case class Grid(numbers: ArrayBuffer[Number], symbols: Set[Position]):
  private var row = 0

  def parseRow(input: String): Unit =
    row += 1
    for found <- digit.findAllMatchIn(input) do
      val pos = Position(row, found.start)
      numbers.append(Number(pos, found.matched))
    for found <- symbol.findAllMatchIn(input) do
      symbols.add(Position(row, found.start))

  def sum: Int =
    numbers
      .filter(num => num.isNear(symbols))
      .map(num => num.text.toInt)
      .sum

case object Grid:
  def empty = Grid(ArrayBuffer(), Set())

case class Number(pos: Position, text: String):
  def adjacent = pos.adjacent(text.length())
  def isNear(symbols: Set[Position]): Boolean =
    adjacent.exists(symbols.contains)

case class Position(row: Int, col: Int):
  def adjacent(size: Int): Seq[Position] =
    for
      i <- row - 1 to row + 1
      j <- col - 1 to col + size
    yield Position(i, j)
