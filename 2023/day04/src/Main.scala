import scala.io.Source

@main def main() =
  val points =
    for line <- Source.fromResource("input.txt").getLines()
    yield Card.parse(line).points
  println(points.sum)

case class Card(winning: Set[String], numbers: Set[String]):
  def points: Int =
    numbers.count(winning.contains) match
      case 0 => 0
      case n => scala.math.pow(2, n - 1).toInt

case object Card:
  def parse(input: String): Card =
    val Array(winning, numbers) =
      input.split(": ").last.split(" \\| ").map(toSet)
    Card(winning, numbers)
  def toSet(input: String): Set[String] =
    input.split(" ").filter(s => s != "").toSet
