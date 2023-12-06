import scala.io.Source

@main def main() =
  val lines = Source.fromResource("input.txt").getLines()
  println(Races.parse(lines.toSeq).multiply)

case class Races(races: Seq[Race]):
  def multiply: Int = races.map(_.count).multiply

case object Races:
  val digits = "[0-9]+".r
  def parse(lines: Iterable[String]): Races =
    val Seq(times, distances) = lines.take(2).map(numbers).toSeq
    val races =
      for (time, distance) <- times.zip(distances)
      yield Race(time, distance)
    Races(races)
  def numbers(input: String): Seq[Int] =
    digits.findAllIn(input).map(_.toInt).toSeq

case class Race(time: Int, distance: Int):
  def count: Int = ways.count(x => x)
  def ways: Seq[Boolean] =
    for hold <- 1 until time
    yield success(hold)
  def success(hold: Int): Boolean =
    (time - hold) * hold > distance

extension (seq: Seq[Int])
  def multiply: Int =
    seq match
      case Seq() => 0
      case s => s.fold(1)(_ * _)
