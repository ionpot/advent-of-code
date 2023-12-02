import scala.io.Source

@main def main() =
  val input =
    for line <- Source.fromResource("input.txt").getLines()
      yield findDigits(line)
  println(calibrate(input.toSeq))

def calibrate(input: Seq[String]): Int =
  val numbers = for str <- input
    yield str.toIntOption match
      case Some(value) => value
      case None => 0
  numbers.sum

val digit = "[0-9]".r

def findDigits(line: String): String =
  val found = digit.findAllMatchIn(line).toSeq
  (found.headOption, found.lastOption) match
    case (Some(head), Some(last)) => head.matched + last.matched
    case _ => ""
