ThisBuild / scalaVersion := "3.3.1"

lazy val baseSettings = Seq(
  libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
  Compile / scalaSource := sourceDirectory.value,
  Compile / resourceDirectory := baseDirectory.value,
  Test / scalaSource := baseDirectory.value / "test",
)

lazy val day01 = (project in file("day01"))
  .settings(name := "day01", baseSettings)
