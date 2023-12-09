import scala.io.Source

@main def main() =
  val lines = Source.fromResource("input.txt").getLines()
  println(DesertMap.parse(lines).steps)

case class DesertMap(directions: Directions, nodes: Map[Label, Node]):
  val start: Label = "AAA"
  val finish: Label = "ZZZ"

  def steps: Int = step(start, 0)

  private def step(current: Label, count: Int): Int =
    if current == finish then count
    else step(next(current), count + 1)

  private def next(label: Label): Label =
    val node = nodes(label)
    if node.infinite then throw Error("infinite node")
    else node.next(directions.next())

case object DesertMap:
  def parse(lines: Iterator[String]): DesertMap =
    val dirs = Directions.parse(lines.next())
    val nodes =
      for line <- lines if line.nonEmpty yield
        val node = Node.parse(line)
        (node.label, node)
    DesertMap(dirs, nodes.toMap)

case class Node(label: Label, nodes: Map[Direction, Label]):
  def next(dir: Direction): Label = nodes(dir)
  def infinite: Boolean =
    nodes.values.forall(_ == label)

case object Node:
  val pattern = "[A-Z]+".r
  def parse(input: String): Node =
    val Seq(label, left, right) = pattern.findAllIn(input).toSeq
    val nodes = Map("L" -> left, "R" -> right)
    Node(label, nodes)

type Label = String

case class Directions(dirs: Seq[Direction])
    extends Iterator[Direction]:
  private var current: Seq[Direction] = dirs

  override def next(): Direction =
    if current.isEmpty then current = dirs
    val dir = current.head
    current = current.tail
    dir

  override def hasNext: Boolean = true

case object Directions:
  def parse(input: String) = Directions(input.split("").toSeq)

type Direction = String
