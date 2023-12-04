ThisBuild / scalaVersion := "3.3.1"
ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation")

lazy val baseSettings = Seq(
  libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
  Compile / scalaSource := sourceDirectory.value,
  Compile / resourceDirectory := baseDirectory.value,
  Test / scalaSource := baseDirectory.value / "test",
)

lazy val day01 = (project in file("day01"))
  .settings(name := "day01", baseSettings)

lazy val day02 = (project in file("day02"))
  .settings(name := "day02", baseSettings)

lazy val day03 = (project in file("day03"))
  .settings(name := "day03", baseSettings)

lazy val day04 = (project in file("day04"))
  .settings(name := "day04", baseSettings)
