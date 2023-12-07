import scala.concurrent.Await
import scala.concurrent.duration.Duration

class MySuite extends munit.FunSuite {
  test("races") {
    val lines = Seq(
      "Time:      7  15   30",
      "Distance:  9  40  200",
    )
    val future = Races.parse(lines.iterator).count
    val count = Await.result(future, Duration.Inf)
    assertEquals(count, 71503)
  }
}
