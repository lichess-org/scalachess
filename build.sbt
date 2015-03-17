name := "scalachess"

organization := "org.lichess"

version := "4.1"

scalaVersion := "2.11.6"

libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.1.1",
  "org.specs2" %% "specs2-core" % "3.0.1" % "test",
  "com.github.ornicar" %% "scalalib" % "5.3",
  "joda-time" % "joda-time" % "2.7"
)

updateOptions := updateOptions.value.withCachedResolution(true)

resolvers ++= Seq(
  "sonatype" at "http://oss.sonatype.org/content/repositories/releases",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases")

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked")
