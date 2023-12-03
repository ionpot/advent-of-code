import scala.io.Source
import scala.collection.immutable.ArraySeq

@main def main() =
  val games =
    for line <- Source.fromResource("input.txt").getLines().toSeq
    yield Game.parse(line)
  println(Game.sum(games, availableHand))

val availableHand = Hand(
  Seq(Ball(12, "red"), Ball(13, "green"), Ball(14, "blue")),
)

case class Game(id: Int, hands: Seq[Hand]):
  def possibleFor(available: Hand): Boolean =
    hands.forall(available.contains)

case object Game:
  def empty = Game(0, Seq())
  def parse(line: String): Game =
    line.split(": ") match
      case Array(key, value) =>
        val id = key.split(" ").lastOption.getOrElse("0")
        val hands =
          for input <- value.splitToSeq("; ")
          yield Hand.parse(input)
        Game(id.toInt, hands)
      case _ => empty
  def sum(games: Seq[Game], available: Hand): Int =
    val ids = for game <- games if game.possibleFor(available)
    yield game.id
    ids.sum

case class Hand(balls: Seq[Ball]):
  def contains(other: Hand): Boolean =
    other.balls.forall(otherBall =>
      balls.exists(thisBall => thisBall.contains(otherBall)),
    )

case object Hand:
  def parse(input: String): Hand =
    Hand(for str <- input.splitToSeq(", ") yield Ball.parse(str))

case class Ball(count: Int, color: String):
  def contains(other: Ball): Boolean =
    if color == other.color
    then count >= other.count
    else false

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
