import snapshot4s.BuildInfo.snapshot4sVersion

inThisBuild(
  Seq(
    scalaVersion := "3.7.3",
    version := "17.12.2",
    organization := "com.github.lichess-org.scalachess",
    licenses += ("MIT" -> url("https://opensource.org/licenses/MIT")),
    publishTo := Option(Resolver.file("file", new File(sys.props.getOrElse("publishTo", "")))),
    semanticdbEnabled := true, // for scalafix
    Compile / packageDoc / publishArtifact := false
  )
)

val scalalibVersion = "11.9.1"

val commonSettings = Seq(
  scalacOptions := Seq(
    "-encoding",
    "utf-8",
    "-rewrite",
    // "-indent",
    "-feature",
    "-language:postfixOps",
    "-Wunused:all",
    "-release:21"
    // "-Werror"
    // Warnings as errors!
    /* "-Xfatal-warnings" */
  )
)

lazy val scalachess: Project = Project("scalachess", file("core")).settings(
  commonSettings,
  name := "scalachess",
  libraryDependencies ++= List(
    "com.github.lichess-org.scalalib" %% "scalalib-core" % scalalibVersion,
    "com.github.lichess-org.scalalib" %% "scalalib-model" % scalalibVersion,
    "org.typelevel" %% "cats-core" % "2.13.0",
    "org.typelevel" %% "alleycats-core" % "2.13.0",
    "org.typelevel" %% "cats-parse" % "1.1.0",
    "dev.optics" %% "monocle-core" % "3.3.0",
    "org.typelevel" %% "kittens" % "3.5.0"
  ),
  resolvers += "jitpack".at("https://jitpack.io")
)

lazy val playJson: Project = Project("playJson", file("playJson"))
  .settings(
    commonSettings,
    name := "scalachess-play-json",
    libraryDependencies ++= List(
      "org.playframework" %% "play-json" % "3.0.5",
      "com.github.lichess-org.scalalib" %% "scalalib-play-json" % scalalibVersion
    )
  )
  .dependsOn(scalachess)

lazy val rating: Project = Project("rating", file("rating"))
  .settings(
    commonSettings,
    name := "scalachess-rating"
  )
  .dependsOn(scalachess)

lazy val tiebreak: Project = Project("tiebreak", file("tiebreak"))
  .settings(
    commonSettings,
    name := "scalachess-tiebreak"
  )
  .dependsOn(scalachess, rating)

lazy val bench = project
  .enablePlugins(JmhPlugin)
  .settings(commonSettings, scalacOptions -= "-Wunused:all", name := "bench")
  .settings(publish := {}, publish / skip := true)
  .disablePlugins(ScalafixPlugin)
  .dependsOn(scalachess, testKit, testKit % "compile->test")

lazy val testKit = project
  .in(file("./test-kit"))
  .enablePlugins(Snapshot4sPlugin)
  .settings(
    commonSettings,
    name := "scalachess-test-kit",
    libraryDependencies ++= List(
      "org.scalacheck" %% "scalacheck" % "1.19.0",
      "org.typelevel" %% "literally" % "1.2.0",
      "org.scalameta" %% "munit" % "1.2.0" % Test,
      "org.scalameta" %% "munit-scalacheck" % "1.2.0" % Test,
      "org.typelevel" %% "weaver-cats" % "0.10.1" % Test,
      "org.typelevel" %% "weaver-scalacheck" % "0.10.1" % Test,
      "co.fs2" %% "fs2-core" % "3.12.2" % Test,
      "co.fs2" %% "fs2-io" % "3.12.2" % Test,
      "org.typelevel" %% "discipline-munit" % "2.0.0" % Test,
      "org.typelevel" %% "cats-laws" % "2.13.0" % Test,
      "com.siriusxm" %% "snapshot4s-munit" % snapshot4sVersion % Test
    )
  )
  .dependsOn(scalachess % "compile->compile", rating % "compile->compile", tiebreak % "compile->compile")

lazy val root = project
  .in(file("."))
  .settings(publish := {}, publish / skip := true)
  .aggregate(scalachess, rating, tiebreak, playJson, testKit, bench)

addCommandAlias("prepare", "scalafixAll; scalafmtAll")
addCommandAlias("check", "; scalafixAll --check; scalafmtCheckAll")
