ThisBuild / scalaVersion := "3.3.1"
ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation")

lazy val baseSettings = Seq(
  libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
  Compile / scalaSource := sourceDirectory.value,
  Compile / resourceDirectory := baseDirectory.value,
  Test / scalaSource := baseDirectory.value / "test",
  Test / resourceDirectory := baseDirectory.value / "test",
)

def p(name: String) =
  Project(name, file(name))
    .settings(baseSettings)

lazy val day01 = p("day01")
lazy val day02 = p("day02")
lazy val day03 = p("day03")
lazy val day04 = p("day04")
lazy val day05 = p("day05")
lazy val day06 = p("day06")
