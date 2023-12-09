import scala.io.Source

class MySuite extends munit.FunSuite {
  test("directions") {
    val dirs = Directions.parse("LR")
    assertEquals(dirs.next(), "L")
    assertEquals(dirs.next(), "R")
    assertEquals(dirs.next(), "L")
    assertEquals(dirs.next(), "R")
  }
  test("node") {
    val node = Node.parse("AAA = (BBB, CCC)")
    assertEquals(node.label, "AAA")
    assertEquals(node.next("L"), "BBB")
    assertEquals(node.next("R"), "CCC")
    assert(!node.infinite)
  }
  test("input-1") {
    val lines = Source.fromResource("input-1.txt").getLines()
    val map = DesertMap.parse(lines)
    assertEquals(map.steps, 2)
  }
  test("input-2") {
    val lines = Source.fromResource("input-2.txt").getLines()
    val map = DesertMap.parse(lines)
    assertEquals(map.steps, 6)
  }
}
