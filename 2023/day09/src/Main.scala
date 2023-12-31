import scala.io.Source

@main def main() =
  val lines = Source.fromResource("input.txt").getLines()
  println(run(lines))

def run(lines: Iterator[String]): Int =
  val predictions =
    for line <- lines if line.nonEmpty
    yield predict(Sequence.parse(line))
  predictions.sum

def predict(sequence: Sequence): Int =
    val values = history(sequence).map(_.first)
    values.head - values.tail.foldRight(0)(_ - _)

def history(
    current: Sequence,
    output: Seq[Sequence] = Seq(),
): Seq[Sequence] =
  if current.allZero then output
  else history(current.subSeq, output :+ current)

case class Sequence(values: Seq[Int]):
  def allZero: Boolean = values.forall(_ == 0)
  def first: Int = values.head
  def subSeq: Sequence =
    Sequence(
      values.sliding(2).map(slice => slice.last - slice.head).toSeq,
    )

case object Sequence:
  def parse(input: String): Sequence =
    Sequence(input.split(" ").map(_.toInt).toSeq)
