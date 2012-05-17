import sbt._
import Keys._

trait Resolvers {
  val iliaz = "iliaz.com" at "http://scala.iliaz.com/"
}

trait Dependencies {
  val scalaz = "org.scalaz" %% "scalaz-core" % "6.0.4"
  val specs2 = "org.specs2" %% "specs2" % "1.8.2"
  val scalalib = "com.github.ornicar" %% "scalalib" % "1.30"
  val hasher = "com.roundeights" % "hasher" % "0.3" from "http://cloud.github.com/downloads/Nycto/Hasher/hasher_2.9.1-0.3.jar"
}

object ApplicationBuild extends Build with Resolvers with Dependencies {

  private val buildSettings = Project.defaultSettings ++ Seq(
    organization := "com.github.ornicar",
    version := "1.1",
    scalaVersion := "2.9.1",
    resolvers := Seq(iliaz),
    libraryDependencies := Seq(scalaz, scalalib, hasher),
    libraryDependencies in test := Seq(specs2),
    shellPrompt := {
      (state: State) ⇒ "%s> ".format(Project.extract(state).currentProject.id)
    },
    scalacOptions := Seq("-deprecation", "-unchecked"),
    publishTo := Some(Resolver.sftp(
      "iliaz",
      "scala.iliaz.com"
    ) as ("scala_iliaz_com", Path.userHome / ".ssh" / "id_rsa"))
  )

  lazy val main = Project("scalachess", file("."), settings = buildSettings)
}
