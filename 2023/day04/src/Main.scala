import scala.io.Source
import scala.collection.mutable.ArrayBuffer

@main def main() =
  val cards = Cards.empty
  for line <- Source.fromResource("input.txt").getLines()
  do cards.add(line)
  println(cards.sum)

case class Cards(totals: ArrayBuffer[Int]):
  private var current = 0
  def sum: Int = totals.sum
  def add(input: String): Unit = add(Card.parse(input))
  def add(card: Card): Unit =
    addTo(current, 1)
    distribute(next = card.matches, copies = totals(current))
    current += 1
  def distribute(next: Int, copies: Int): Unit =
    if next > 0 then
      for i <- current + 1 to current + next
      do addTo(i, copies)
  def addTo(i: Int, copies: Int): Unit =
    if totals.length == i then totals.addOne(0)
    totals(i) += copies

case object Cards:
  def empty = Cards(ArrayBuffer())

case class Card(winning: Set[String], numbers: Set[String]):
  def matches: Int = numbers.count(winning.contains)

case object Card:
  def parse(input: String): Card =
    val Array(winning, numbers) =
      input.split(": ").last.split(" \\| ").map(toSet)
    Card(winning, numbers)
  def toSet(input: String): Set[String] =
    input.split(" ").filter(s => s != "").toSet
