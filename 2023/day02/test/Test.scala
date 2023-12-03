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
  test("hand contains") {
    val hand1 = Hand(Seq(Ball(1, "red"), Ball(1, "blue")))
    val hand2 = Hand(Seq(Ball(2, "red"), Ball(2, "blue")))
    val hand3 = Hand(Seq(Ball(2, "red"), Ball(2, "green")))
    assert(hand1.contains(hand1))
    assert(!hand1.contains(hand2))
    assert(!hand1.contains(hand3))
    assert(hand2.contains(hand1))
    assert(hand2.contains(hand2))
    assert(!hand2.contains(hand3))
    assert(!hand3.contains(hand1))
    assert(!hand3.contains(hand2))
    assert(hand3.contains(hand3))
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
    val sum = Game.sum(games, availableHand)
    assertEquals(sum, 8)
  }
}
