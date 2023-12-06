class MySuite extends munit.FunSuite {
  test("races") {
    val lines = Seq(
      "Time:      7  15   30",
      "Distance:  9  40  200",
    )
    assertEquals(Races.parse(lines).multiply, 288)
  }
}
