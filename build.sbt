val commonSettings = Seq(
  scalaVersion := "2.12.8",
  scalacOptions += "-Ypartial-unification",
  libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0",
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.27"
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
