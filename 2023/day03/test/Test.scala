import scala.collection.mutable.HashMap

class MySuite extends munit.FunSuite {
  test("adjacent") {
    val p = Position(1, 1)
    val actual = p
      .adjacent(size = 1)
      .sortWith((a, b) =>
        if (a.row == b.row) a.col < b.col else a.row < b.row,
      )
    val expected = Seq(
      Position(0, 0),
      Position(0, 1),
      Position(0, 2),
      Position(1, 0),
      Position(1, 1),
      Position(1, 2),
      Position(2, 0),
      Position(2, 1),
      Position(2, 2),
    )
    assertEquals(actual, expected)
  }
  test("grid") {
    val rows = Seq(
      "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598..",
    )
    val grid = Grid.empty
    for row <- rows do grid.parseRow(row)
    assertEquals(grid.sum, 4361)
  }
}
