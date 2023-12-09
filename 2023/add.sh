#!/bin/bash

dir="day$1"

cat >>build.sbt <<SBT
lazy val $dir = p("$dir")
SBT

mkdir $dir
mv ~/Downloads/input.txt $dir

mkdir $dir/src
cat >$dir/src/Main.scala <<CODE
import scala.io.Source

@main def main() =
  val lines = Source.fromResource("input.txt").getLines()
  println(run(lines))
CODE

mkdir $dir/test
cat >$dir/test/Test.scala <<CODE
class MySuite extends munit.FunSuite {
  test("input") {
  }
}
CODE
