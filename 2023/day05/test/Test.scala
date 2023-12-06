import scala.io.Source

class MySuite extends munit.FunSuite {
  test("almanac lowest") {
    val lines = Source.fromResource("input.txt").getLines()
    val alm = Almanac.empty
    alm.parse(lines)
    assertEquals(alm.lowest, 35L)
  }
}
