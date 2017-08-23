name := "scalachess"

organization := "org.lichess"

version := "6.5"

scalaVersion := "2.11.11"
crossScalaVersions := Seq("2.11.11", "2.12.3")

libraryDependencies ++= List(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.scalaz" %% "scalaz-core" % "7.2.13",
  "org.specs2" %% "specs2-core" % "3.9.0" % "test",
  "com.github.ornicar" %% "scalalib" % "6.3",
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
  // "-Ywarn-unused:-params,_",
  "-Xlint:missing-interpolator",
  "-Ydelambdafy:method", "-target:jvm-1.8")

publishTo := Some(Resolver.file("file",  new File(sys.props.getOrElse("publishTo", ""))))

import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

val preferences =
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(CompactControlReadability, true)
  .setPreference(DoubleIndentConstructorArguments, true)
  .setPreference(DanglingCloseParenthesis, Force)

Seq(preferences)
