name := "scalachess"

organization := "org.lichess"

version := "5.4"

scalaVersion := "2.11.11"

libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.1.11",
  "org.specs2" %% "specs2-core" % "3.6" % "test",
  "com.github.ornicar" %% "scalalib" % "5.7",
  "joda-time" % "joda-time" % "2.9.7",
  "org.joda" % "joda-convert" % "1.8",
  "org.scala-lang.modules" %% "scala-java8-compat" % "0.8.0")

// updateOptions := updateOptions.value.withCachedResolution(true)

resolvers ++= Seq(
  "lila-maven" at "https://raw.githubusercontent.com/ornicar/lila-maven/master",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases")

scalacOptions ++= Seq(
  "-deprecation", "-unchecked", "-feature", "-language:_",
  "-Xfatal-warnings",
  "-Ywarn-unused-import", "-Ywarn-value-discard", "-Ywarn-dead-code",
  "-Ywarn-unused", "-Xlint:missing-interpolator",
  "-Ybackend:GenBCode", "-Ydelambdafy:method", "-target:jvm-1.8")

import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

SbtScalariform.scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(CompactControlReadability, true)
  .setPreference(DoubleIndentClassDeclaration, true)
