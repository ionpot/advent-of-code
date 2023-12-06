import scala.io.Source
import concurrent.ExecutionContext.Implicits.global

class MySuite extends munit.FunSuite {
  test("almanac lowest") {
    val lines = Source.fromResource("input.txt").getLines()
    val alm = Almanac.empty
    alm.parse(lines)
    for lowest <- alm.lowest
    do assertEquals(lowest, 46L)
  }
}
