import sbt._, Keys._

trait Resolvers {
  val iliaz = "iliaz.com" at "http://scala.iliaz.com/"
  val sonatype = "sonatype" at "http://oss.sonatype.org/content/repositories/releases"
  val awesomepom = "awesomepom" at "https://raw.github.com/jibs/maven-repo-scala/master"
  val roundeights = "RoundEights" at "http://maven.spikemark.net/roundeights"
}

trait Dependencies {
  val scalaz = "org.scalaz" %% "scalaz-core" % "7.0.6"
  val specs2 = "org.specs2" %% "specs2" % "2.3.12" % "test"
  val scalalib = "com.github.ornicar" %% "scalalib" % "5.0"
  val jodaTime = "joda-time" % "joda-time" % "2.3"
}

object ApplicationBuild extends Build with Resolvers with Dependencies {

  private val buildSettings = Project.defaultSettings ++ Seq(
    scalaVersion := "2.11.2",
    organization := "org.lichess",
    version := "4.0",
    resolvers := Seq(iliaz, sonatype, awesomepom),
    libraryDependencies := Seq(scalaz, scalalib, jodaTime, specs2),
    incOptions := incOptions.value.withNameHashing(true),
    scalacOptions := Seq(
      "-deprecation",
      "-unchecked",
      "-feature",
      "-language:_"),
    publishTo := Some(Resolver.sftp(
      "iliaz",
      "scala.iliaz.com"
    ) as ("scala_iliaz_com", Path.userHome / ".ssh" / "id_rsa"))
  )

  lazy val main = Project("scalachess", file("."), settings = buildSettings)
}
