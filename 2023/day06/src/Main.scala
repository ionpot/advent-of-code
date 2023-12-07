import scala.io.Source
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global

@main def main() =
  val lines = Source.fromResource("input.txt").getLines()
  val count = Races.parse(lines).count
  println(Await.result(count, Duration.Inf))

case class Races(time: Int, distance: Long):
  def count: Future[Int] =
    Future.sequence(races).map(_.sum)

  def races: Seq[Future[Int]] =
    val chunk = 1_000_000
    for i <- 0 to time / chunk
    yield Future(toRace(i, chunk).count)

  def toRace(i: Int, chunk: Int): Race =
    val a = i * chunk
    val b = a + chunk
    val range = Range(a.max(1), b.min(time))
    Race(BigInt(time), BigInt(distance), range)

case object Races:
  val digits = "[0-9]+".r

  def parse(lines: Iterator[String]): Races =
    val time = number(lines.next()).toInt
    val distance = number(lines.next()).toLong
    Races(time, distance)

  def number(input: String): String =
    digits.findAllIn(input).mkString

case class Race(time: BigInt, distance: BigInt, range: Range):
  def count: Int =
    var count = 0
    for hold <- range if success(hold)
    do count += 1
    count

  def success(hold: Int): Boolean =
    val h = BigInt(hold)
    (time - h) * h > distance
