/**
 * Copyright (C) 2012 Typesafe, Inc. <http://www.typesafe.com>
 */

import sbt._
import sbt.Keys._

object ZincBuild extends Build {
  val sbtSuffix = 15
  val zincSuffix = 18

  val sbtVersion = s"0.13.7-JAR-OUTPUT-${sbtSuffix}"

  val resolveSbtLocally = settingKey[Boolean]("resolve-sbt-locally")

  lazy val buildSettings = Seq(
    organization := "com.typesafe.zinc",
    version := s"0.3.7-JAR-OUTPUT-${sbtSuffix}-${zincSuffix}",
    scalaVersion := "2.10.4",
    crossPaths := false
  )

  lazy val zinc = Project(
    "zinc",
    file("."),
    settings = buildSettings ++ Version.settings ++ Publish.settings ++ Dist.settings ++ Scriptit.settings ++ Seq(
      resolveSbtLocally := true,
      resolvers += (if (resolveSbtLocally.value) Resolver.mavenLocal else Opts.resolver.sonatypeSnapshots),
      libraryDependencies ++= Seq(
        "org.scala-sbt" % "incremental-compiler" % sbtVersion,
        "org.scala-sbt" % "compiler-interface" % sbtVersion classifier "sources",
        "com.martiansoftware" % "nailgun-server" % "0.9.1" % "optional"
      ),
      scalacOptions ++= Seq("-feature", "-deprecation", "-Xlint")
    )
  )
}
