/* Any copyright is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/ */

name := "maia"
version := "1.0"
scalaVersion := "2.12.1"
scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-unchecked",
  "-deprecation",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen"
)
licenses := Seq("MPLv2" -> url("http://mozilla.org/MPL/2.0/"))
homepage := Some(url("http://github.com/tel/scala-maia"))
resolvers += Resolver.sonatypeRepo("releases")

val catsVersion = "0.9.0"
val circeVersion = "0.7.0"
val shapelessVersion = "2.3.2"

val uTestVersion = "0.4.5"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.typelevel" %% "cats" % catsVersion,
  "com.chuusai" %% "shapeless" % shapelessVersion
)

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "utest" % uTestVersion % "test",
  "co.fs2" %% "fs2-core" % "0.9.2" % "test",
  "co.fs2" %% "fs2-cats" % "0.3.0"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

testFrameworks +=
  new TestFramework("utest.runner.Framework")

enablePlugins(SiteScaladocPlugin)
enablePlugins(AsciidoctorPlugin)

wartremoverErrors ++= Warts.allBut(
  Wart.Any,
  Wart.AsInstanceOf,
  Wart.ExplicitImplicitTypes,
  Wart.Nothing
)

scalastyleSources in Test ++= (unmanagedSourceDirectories in Compile).value
scalastyleSources in Test ++= (unmanagedSourceDirectories in Test).value
scalastyleFailOnError := true

// The following lines enable automatic ScalaStyle linting during tests
lazy val testScalastyle = taskKey[Unit]("testScalastyle")
testScalastyle := org.scalastyle.sbt.ScalastylePlugin.scalastyle
  .in(Test)
  .toTask("")
  .value
(test in Test) := ((test in Test) dependsOn testScalastyle).value
