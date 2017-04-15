/* Any copyright is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/ */

val catsVersion = "0.9.0"
val circeVersion = "0.7.0"
val shapelessVersion = "2.3.2"
val uTestVersion = "0.4.5"

val http4sVersion = "0.17.0-SNAPSHOT"
val fs2Version = "0.9.4"
val fs2CatsVersion = "0.3.0"

lazy val commonSettings = Seq(
  version := "1.0",
  organization := "com.jspha",
  scalaVersion := "2.12.1",
  scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-unchecked",
    "-deprecation",
    "-Yno-adapted-args",
    "-Ywarn-numeric-widen"
  ),
  licenses := Seq("MPLv2" -> url("http://mozilla.org/MPL/2.0/")),
  homepage := Some(url("http://github.com/MaiaOrg/scala-maia")),
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  libraryDependencies ++= Seq(
    "io.circe" %%% "circe-core" % circeVersion,
    "io.circe" %%% "circe-generic" % circeVersion,
    "io.circe" %%% "circe-parser" % circeVersion,
    "org.typelevel" %%% "cats" % catsVersion,
    "com.chuusai" %%% "shapeless" % shapelessVersion
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(maiaJS,
             maiaJVM,
             maiaHttp4s,
             exampleAppSharedJS,
             exampleAppSharedJVM,
             exampleAppUi,
             exampleAppServer)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val maia = crossProject
  .crossType(CrossType.Pure)
  .in(file("maia"))
  .settings(commonSettings: _*)
  .settings(
    name := "maia",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "utest" % uTestVersion % "test"
    ),
    testFrameworks +=
      new TestFramework("utest.runner.Framework"),
    wartremoverErrors ++= Warts.allBut(
      Wart.Any,
      Wart.AsInstanceOf,
      Wart.ExplicitImplicitTypes,
      Wart.Nothing,
      Wart.Overloading
    )
  )

lazy val maiaJVM = maia.jvm
lazy val maiaJS = maia.js

// The following lines enable automatic ScalaStyle linting during tests
lazy val testScalastyle = taskKey[Unit]("testScalastyle")
testScalastyle := org.scalastyle.sbt.ScalastylePlugin.scalastyle
  .in(Test)
  .toTask("")
  .value
(test in Test) := ((test in Test) dependsOn testScalastyle).value

// ----------------------------------------------------------------------------
// Auxiliary Libraries

lazy val maiaHttp4s = project
  .in(file("maia-http4s"))
  .dependsOn(maiaJVM)
  .settings(commonSettings: _*)
  .settings(
    name := "maia-http4s",
    wartremoverErrors ++= Warts.allBut(
      Wart.Any,
      Wart.AsInstanceOf,
      Wart.ExplicitImplicitTypes,
      Wart.Nothing,
      Wart.Overloading
    ),
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "co.fs2" %% "fs2-cats" % fs2CatsVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion
    )
  )

// ----------------------------------------------------------------------------
// Example App

lazy val exampleAppShared = crossProject
  .crossType(CrossType.Pure)
  .in(file("example-app/shared"))
  .dependsOn(maia)
  .jvmConfigure(_.dependsOn(maiaJVM))
  .jsConfigure(_.dependsOn(maiaJS))
  .settings(commonSettings: _*)

lazy val exampleAppSharedJVM =
  exampleAppShared.jvm.settings(name := "sharedJvm")
lazy val exampleAppSharedJS =
  exampleAppShared.js.settings(name := "sSaredJs")

lazy val exampleAppUi = project
  .in(file("example-app/ui"))
  .settings(commonSettings: _*)
  .dependsOn(exampleAppSharedJS)
  .dependsOn(maiaJS)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    skip in packageJSDependencies := false,
    libraryDependencies ++= Seq(
      "fr.hmil" %%% "roshttp" % "2.0.1"
    )
  )
  .settings(workbenchSettings)
  .settings(
    bootSnippet := "jspha.qubit.ui.Runtime().main();",
    refreshBrowsers := {
      refreshBrowsers.triggeredBy(fastOptJS in Compile).value
    }
  )

lazy val exampleAppServer = project
  .in(file("example-app/server"))
  .settings(commonSettings: _*)
  .dependsOn(maiaJVM)
  .dependsOn(maiaHttp4s)
  .dependsOn(exampleAppSharedJVM)
  .settings(
    libraryDependencies ++= Seq(
      "org.slf4j" % "slf4j-simple" % "1.6.4",
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "co.fs2" %% "fs2-cats" % fs2CatsVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion
    )
  )
  .settings( // bring ui compiled artifacts in to resources
    (resources in Compile) += Def
      .task {
        (artifactPath in (exampleAppUi, Compile, fullOptJS)).value
      }
      .dependsOn(fullOptJS in (exampleAppUi, Compile))
      .value,
    (resources in Compile) += Def
      .task {
        (artifactPath in (exampleAppUi, Compile, packageMinifiedJSDependencies)).value
      }
      .dependsOn(fullOptJS in (exampleAppUi, Compile))
      .value
  )
