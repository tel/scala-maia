/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

name := "comms"
version := "1.0"
scalaVersion := "2.11.8"
licenses := Seq("MPLv2" -> url("http://mozilla.org/MPL/2.0/"))
homepage := Some(url("http://github.com/tel/scala-comms"))

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
