class MySuite extends munit.FunSuite {
  test("parse game") {
    val game = Game.parse(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    )
    assertEquals(game.id, 1)
    val Seq(h1, h2, h3) = game.hands
    val Seq(b1, b2) = h1.balls
    val Seq(b3, b4, b5) = h2.balls
    val Seq(b6) = h3.balls
    assertEquals(b1, Ball(3, "blue"))
    assertEquals(b2, Ball(4, "red"))
    assertEquals(b3, Ball(1, "red"))
    assertEquals(b4, Ball(2, "green"))
    assertEquals(b5, Ball(6, "blue"))
    assertEquals(b6, Ball(2, "green"))
  }
  test("ball contains") {
    assert(Ball(1, "red").contains(Ball(0, "red")))
    assert(Ball(1, "red").contains(Ball(1, "red")))
    assert(!Ball(1, "red").contains(Ball(2, "red")))
    assert(!Ball(1, "red").contains(Ball(1, "blue")))
  }
  test("hands max") {
    val inputs = Seq(
      (1, "1 red"),
      (3, "1 red; 2 red; 3 red"),
      (3, "3 red; 2 red; 1 red"),
      (24, "4 red; 3 green; 2 blue"),
    )
    for (power, input) <- inputs do
      val hands = Hand.parseSeq(input)
      assertEquals(Hand.highest(hands).power, power)
  }
  test("sum games") {
    val inputs = Seq(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
    )
    val games = for input <- inputs yield Game.parse(input)
    val sum = Game.sum(games)
    assertEquals(sum, 2286)
  }
}
