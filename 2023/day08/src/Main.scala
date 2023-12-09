import scala.io.Source

@main def main() =
  val lines = Source.fromResource("input.txt").getLines()
  println(run(lines))

def run(lines: Iterator[String]): BigInt =
  val chars = lines.next().split("").toSeq
  val desert = DesertMap.parse(lines)
  val ghosts = desert.startNodes
    .map(desert.steps(_, Directions(chars)))
    .map(BigInt(_))
  ghosts.tail.fold(ghosts.head)(lcm)

case class DesertMap(nodes: Map[Label, Node]):
  def startNodes: Seq[Node] = nodes.values.filter(_.start).toSeq
  def steps(start: Node, directions: Directions): Int =
    step(start, directions, 0)

  private def step(node: Node, dirs: Directions, count: Int): Int =
    if node.finish then count
    else step(next(node, dirs.next()), dirs, count + 1)

  private def next(node: Node, dir: Direction): Node =
    if node.infinite then throw Error("infinite node")
    else nodes(node.next(dir))

case object DesertMap:
  def parse(lines: Iterator[String]): DesertMap =
    val nodes =
      for line <- lines if line.nonEmpty yield
        val node = Node.parse(line)
        (node.label, node)
    DesertMap(nodes.toMap)

case class Node(label: Label, nodes: Map[Direction, Label]):
  def start: Boolean = label.endsWith("A")
  def finish: Boolean = label.endsWith("Z")
  def next(dir: Direction): Label = nodes(dir)
  def infinite: Boolean =
    nodes.values.forall(_ == label)

case object Node:
  val pattern = "[0-9A-Z]+".r
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

type Direction = String

def lcm(a: BigInt, b: BigInt): BigInt =
  a * (b / gcd(a, b))

def gcd(a: BigInt, b: BigInt): BigInt =
  if b <= 0 then a
  else gcd(b, a % b)
