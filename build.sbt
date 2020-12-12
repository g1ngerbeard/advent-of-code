name := "advent-of-code"

version := "0.1"

scalaVersion := "2.13.4"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-Ypartial-unification"
)

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.2",
  "com.lihaoyi" %% "fastparse" % "2.2.2",
  "org.typelevel" %% "cats-core" % "2.3.0",
  "org.scalatest" %% "scalatest" % "3.2.2" % Test
)
