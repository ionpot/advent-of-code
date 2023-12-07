import scala.io.Source

@main def main() =
  val lines = Source.fromResource("input.txt").getLines()
  println(Game.parse(lines).winnings)

case class Game(players: Seq[Player]):
  def winnings: Int =
    players
      .sortWith((a, b) => b.hand.higherThan(a.hand))
      .zipWithIndex
      .map((player, i) => (i + 1) * player.bid)
      .sum

case object Game:
  def parse(lines: Iterator[String]): Game =
    val players =
      for line <- lines if line.nonEmpty
      yield Player.parse(line)
    Game(players.toSeq)

case class Player(hand: Hand, bid: Int)

case object Player:
  def parse(input: String): Player =
    val Array(hand, bid) = input.split(" ")
    Player(Hand.parse(hand), bid.toInt)

case class Hand(cards: Seq[Card]):
  def higherThan(other: Hand): Boolean =
    if typeValue == other.typeValue
    then higherThan(other.cards)
    else typeValue > other.typeValue

  def higherThan(other: Seq[Card]): Boolean =
    cards.zip(other).find((a, b) => a.label != b.label) match
      case Some(a, b) => a.higherThan(b)
      case None => throw IllegalArgumentException("same hand")

  def typeValue = getType.ordinal

  def getType: HandType =
    cards.groupBy(_.label).values.map(_.length).toSeq.sorted match
      case Seq(5) => HandType.FiveKind
      case Seq(1, 4) => HandType.FourKind
      case Seq(2, 3) => HandType.FullHouse
      case Seq(1, 1, 3) => HandType.ThreeKind
      case Seq(1, 2, 2) => HandType.TwoPair
      case Seq(1, 1, 1, 2) => HandType.OnePair
      case Seq(1, 1, 1, 1, 1) => HandType.HighCard
      case other =>
        throw IllegalArgumentException(other.mkString(", "))

case object Hand:
  def parse(input: String): Hand =
    val cards = for label <- input.split("") yield Card(label)
    Hand(cards.toSeq)

enum HandType:
  case HighCard, OnePair, TwoPair, ThreeKind, FullHouse, FourKind,
    FiveKind

case class Card(label: String):
  def value = Card.labels.indexOf(label)
  def higherThan(other: Card) = value > other.value

case object Card:
  val labels = Seq(
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "T",
    "J",
    "Q",
    "K",
    "A",
  )
