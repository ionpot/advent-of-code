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

val digits = Seq(
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine",
)

def convert(digit: String): String =
  val i = digits.indexOf(digit) + 1
  if (i == 0) digit else i.toString()

val digit = Seq("[0-9]").concat(digits).mkString("|").r

def findLastIn(input: String, last: String = ""): String =
  digit.findFirstMatchIn(input) match
    case Some(value) =>
      val sub = input.substring(value.start + 1)
      findLastIn(sub, value.matched)
    case None => last

def findDigits(line: String): String =
  digit.findFirstIn(line) match
    case Some(found) =>
      val last = findLastIn(line)
      convert(found) + convert(last)
    case _ => ""
