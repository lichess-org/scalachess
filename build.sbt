name := "scalachess"

organization := "org.lichess"

version := "6.24"

scalaVersion := "2.12.3"
crossScalaVersions := Seq("2.11.11", "2.12.3")

libraryDependencies ++= List(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.scalaz" %% "scalaz-core" % "7.2.15",
  "org.specs2" %% "specs2-core" % "3.9.2" % "test",
  "com.github.ornicar" %% "scalalib" % "6.5",
  "joda-time" % "joda-time" % "2.9.9"
)

resolvers ++= Seq(
  "lila-maven" at "https://raw.githubusercontent.com/ornicar/lila-maven/master",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

scalacOptions ++= Seq(
  "-deprecation", "-unchecked", "-feature", "-language:_",
  "-Xfatal-warnings",
  "-Ywarn-unused-import", "-Ywarn-value-discard", "-Ywarn-dead-code",
  "-Ywarn-unused:-params,_",
  "-Xlint:missing-interpolator",
  "-Ydelambdafy:method", "-target:jvm-1.8"
)

// dark magic to keep scalafix away from test code
scalafix := ScalafixPlugin.scalafixTaskImpl(Seq(Compile)).evaluated

publishTo := Some(Resolver.file("file", new File(sys.props.getOrElse("publishTo", ""))))

import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

val preferences =
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(DoubleIndentConstructorArguments, true)
    .setPreference(DanglingCloseParenthesis, Force)

Seq(preferences)
