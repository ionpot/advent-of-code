class MySuite extends munit.FunSuite {
  test("find digits") {
    val pairs = List(
      ("1abc2", "12"),
      ("pqr3stu8vwx", "38"),
      ("a1b2c3d4e5f", "15"),
      ("treb7uchet", "77"),
    )
    for (input, expected) <- pairs do
      assertEquals(findDigits(input), expected)
  }
  test("calibrate") {
    assertEquals(calibrate(Seq()), 0)
    assertEquals(calibrate(Seq("12", "3", "ignore")), 15)
  }
}
