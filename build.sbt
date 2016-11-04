/* Any copyright is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/ */

name := "comms"
version := "1.0"
scalaVersion := "2.11.8"
licenses := Seq("MPLv2" -> url("http://mozilla.org/MPL/2.0/"))
homepage := Some(url("http://github.com/tel/scala-comms"))

val catsVersion = "0.7.2"
val circeVersion = "0.5.1"
val shapelessVersion = "2.3.2"
val uTestVersion = "0.4.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.typelevel" %% "cats" % catsVersion,
  "com.chuusai" %% "shapeless" % shapelessVersion,
  "com.lihaoyi" %% "utest" % uTestVersion % "test"
)

testFrameworks +=
  new TestFramework("utest.runner.Framework")
