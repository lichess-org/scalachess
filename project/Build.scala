import sbt._
import Keys._

trait Resolvers {
  val iliaz = "iliaz.com" at "http://scala.iliaz.com/"
  val sonatype = "sonatype" at "http://oss.sonatype.org/content/repositories/releases"
}

trait Dependencies {
  val scalaz = "org.scalaz" % "scalaz-core_2.10.0-RC2" % "6.0.4"
  val specs2 = "org.specs2" %% "specs2" % "1.12.2"
  val scalalib = "com.github.ornicar" % "scalalib_2.9.1" % "2.5"
  val hasher = "com.roundeights" % "hasher" % "0.3" from "http://cloud.github.com/downloads/Nycto/Hasher/hasher_2.9.1-0.3.jar"
  val jodaTime = "joda-time" % "joda-time" % "2.1"
  val jodaConvert = "org.joda" % "joda-convert" % "1.2"
}

object ApplicationBuild extends Build with Resolvers with Dependencies {

  private val buildSettings = Project.defaultSettings ++ Seq(
    scalaVersion := "2.10.0-RC1",
    organization := "com.github.ornicar",
    version := "3",
    resolvers := Seq(iliaz, sonatype),
    libraryDependencies := Seq(scalaz, scalalib, hasher, jodaTime, jodaConvert),
    libraryDependencies in test := Seq(specs2),
    shellPrompt := {
      (state: State) ⇒ "%s> ".format(Project.extract(state).currentProject.id)
    },
    scalacOptions := Seq(
      "-deprecation", 
      "-unchecked", 
      "-feature",
      "-language:implicitConversions",
      "-language:reflectiveCalls",
      "-language:postfixOps"),
    publishTo := Some(Resolver.sftp(
      "iliaz",
      "scala.iliaz.com"
    ) as ("scala_iliaz_com", Path.userHome / ".ssh" / "id_rsa"))
  )

  lazy val main = Project("scalachess", file("."), settings = buildSettings)
}
