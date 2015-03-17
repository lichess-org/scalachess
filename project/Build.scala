import sbt._, Keys._

trait Resolvers {
  val sonatype = "sonatype" at "http://oss.sonatype.org/content/repositories/releases"
  val bintray = "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
}

trait Dependencies {
  val scalaz = "org.scalaz" %% "scalaz-core" % "7.1.1"
  val specs2 = "org.specs2" %% "specs2-core" % "3.0.1" % "test"
  val scalalib = "com.github.ornicar" %% "scalalib" % "5.3"
  val jodaTime = "joda-time" % "joda-time" % "2.7"
}

object ApplicationBuild extends Build with Resolvers with Dependencies {

  private val buildSettings = Project.defaultSettings ++ Seq(
    scalaVersion := "2.11.6",
    organization := "org.lichess",
    version := "4.1",
    resolvers := Seq(bintray),
    libraryDependencies := Seq(scalaz, scalalib, jodaTime, specs2),
    updateOptions := updateOptions.value.withCachedResolution(true),
    scalacOptions := Seq(
      "-deprecation",
      "-unchecked",
      "-feature",
      "-language:_"))

  lazy val main = Project("scalachess", file("."), settings = buildSettings)
}
