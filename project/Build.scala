import sbt._, Keys._

trait Resolvers {
  val iliaz = "iliaz.com" at "http://scala.iliaz.com/"
  val sonatype = "sonatype" at "http://oss.sonatype.org/content/repositories/releases"
  val awesomepom = "awesomepom" at "https://raw.github.com/jibs/maven-repo-scala/master"
}

trait Dependencies {
  val scalaz = "org.scalaz" %% "scalaz-core" % "7.0.3"
  val specs2 = "org.specs2" %% "specs2" % "1.14"
  val scalalib = "com.github.ornicar" %% "scalalib" % "4.19"
  val hasher = "hasher" %% "hasher" % "0.3.1" 
  val jodaTime = "joda-time" % "joda-time" % "2.1"
  val jodaConvert = "org.joda" % "joda-convert" % "1.2"
}

object ApplicationBuild extends Build with Resolvers with Dependencies {

  private val buildSettings = Project.defaultSettings ++ Seq(
    scalaVersion := "2.10.2",
    organization := "org.lichess",
    version := "3.6",
    resolvers := Seq(iliaz, sonatype, awesomepom),
    libraryDependencies := Seq(scalaz, scalalib, hasher, jodaTime, jodaConvert),
    libraryDependencies in test := Seq(specs2),
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
