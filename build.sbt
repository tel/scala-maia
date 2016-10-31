
name := "comms"
version := "1.0"
scalaVersion := "2.11.8"

val catsVersion = "0.7.2"
val circeVersion = "0.5.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.typelevel" %% "cats" % catsVersion,
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.lihaoyi" %% "utest" % "0.4.3" % "test"
)

testFrameworks +=
  new TestFramework("utest.runner.Framework")
