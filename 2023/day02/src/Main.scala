import scala.io.Source
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

@main def main() =
  val games =
    for line <- Source.fromResource("input.txt").getLines().toSeq
    yield Game.parse(line)
  println(Game.sum(games))

case class Game(id: Int, hands: Seq[Hand]):
  def power = Hand.highest(hands).power

case object Game:
  def empty = Game(0, Seq())
  def parse(line: String): Game =
    line.split(": ") match
      case Array(key, value) =>
        val id = key.split(" ").lastOption.getOrElse("0")
        Game(id.toInt, Hand.parseSeq(value))
      case _ => empty
  def sum(games: Seq[Game]): Int =
    games.map(g => g.power).sum

case class Hand(balls: Seq[Ball]):
  def power: Int = balls.map(b => b.count).multiply

case object Hand:
  def empty = Hand(Seq())
  def parse(input: String): Hand =
    Hand(for str <- input.splitToSeq(", ") yield Ball.parse(str))
  def parseSeq(input: String): Seq[Hand] =
    for part <- input.splitToSeq("; ")
    yield Hand.parse(part)
  def highest(hands: Seq[Hand]): Hand =
    val balls = ArrayBuffer[Ball]()
    for ball <- hands.flatMap(h => h.balls) do
      val i = balls.indexWhere(b => b.sameColor(ball))
      if i == -1 then balls.append(ball)
      else if ball.contains(balls(i)) then balls.update(i, ball)
    Hand(balls.toSeq)

case class Ball(count: Int, color: String):
  def contains(other: Ball): Boolean =
    if sameColor(other)
    then count >= other.count
    else false
  def sameColor(other: Ball): Boolean =
    color == other.color

case object Ball:
  def empty = Ball(0, "")
  def parse(input: String): Ball =
    input.split(" ") match
      case Array(count, color) =>
        Ball(count.toIntOption.getOrElse(0), color)
      case _ => empty

extension (s: String)
  def splitToSeq(delim: String): Seq[String] =
    ArraySeq.unsafeWrapArray(s.split(delim))

extension (seq: Seq[Int])
  def multiply: Int =
    seq match
      case Seq() => 0
      case s => s.fold(1)(_ * _)
