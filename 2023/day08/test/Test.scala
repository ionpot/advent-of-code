import scala.io.Source

class MySuite extends munit.FunSuite {
  test("directions") {
    val dirs = Directions(Seq("L", "R"))
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
  test("input") {
    val lines = Source.fromResource("input.txt").getLines()
    assertEquals(run(lines), BigInt(6))
  }
}
