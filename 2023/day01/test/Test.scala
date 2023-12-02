class MySuite extends munit.FunSuite {
  test("find digits") {
    val pairs = List(
      ("two1nine", "29"),
      ("eightwothree", "83"),
      ("abcone2threexyz", "13"),
      ("xtwone3four", "24"),
      ("4nineeightseven2", "42"),
      ("zoneight234", "14"),
      ("7pqrstsixteen", "76"),
      ("oneight", "18"),
    )
    for (input, expected) <- pairs do
      assertEquals(findDigits(input), expected)
  }
  test("calibrate") {
    assertEquals(calibrate(Seq()), 0)
    assertEquals(calibrate(Seq("12", "3", "ignore")), 15)
  }
}
