name := "advent-of-code"

version := "0.1"

scalaVersion := "2.13.4"

scalacOptions ++= Seq(
  "-language:postfixOps"
)

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.2",
  "org.scalatest" %% "scalatest" % "3.2.2" % Test
)
