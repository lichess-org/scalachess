name := "scalachess"

organization := "org.lichess"

version := "9.0.25"

scalaVersion := "2.13.0"

libraryDependencies ++= List(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalaz" %% "scalaz-core" % "7.2.28",
  "org.specs2" %% "specs2-core" % "4.7.0" % "test",
  "org.specs2" %% "specs2-scalaz" % "4.7.0" % "test",
  "com.github.ornicar" %% "scalalib" % "6.7",
  "joda-time" % "joda-time" % "2.10.3"
)

resolvers ++= Seq(
  "lila-maven" at "https://raw.githubusercontent.com/ornicar/lila-maven/master",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

scalacOptions ++= Seq(
  "-deprecation", "-unchecked", "-feature", "-language:_",
  "-Xfatal-warnings",
  "-Ywarn-value-discard", "-Ywarn-dead-code",
  // "-Ywarn-unused:-params,_",
  "-Xlint:missing-interpolator",
  "-Ydelambdafy:method", "-target:jvm-1.8"
)

publishTo := Some(Resolver.file("file", new File(sys.props.getOrElse("publishTo", ""))))

import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

val preferences =
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(DoubleIndentConstructorArguments, true)
    .setPreference(DanglingCloseParenthesis, Force)

Seq(preferences)

excludeFilter in scalariformFormat := "FullOpeningPart*" || "EcopeningDB.scala" || "Fixtures.scala"
