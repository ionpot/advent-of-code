class MySuite extends munit.FunSuite {
  test("winnings") {
    val lines = Seq(
      "32T3K 765",
      "T55J5 684",
      "KK677 28",
      "KTJJT 220",
      "QQQJA 483",
    )
    val game = Game.parse(lines.iterator)
    assertEquals(game.winnings, 6440)
  }
}
