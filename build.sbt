inThisBuild(
  Seq(
    scalaVersion      := "3.6.4",
    version           := "17.6.0-RC1",
    organization      := "org.lichess",
    licenses += ("MIT" -> url("https://opensource.org/licenses/MIT")),
    publishTo         := Option(Resolver.file("file", new File(sys.props.getOrElse("publishTo", "")))),
    semanticdbEnabled := true, // for scalafix
    Compile / packageDoc / publishArtifact := false
  )
)

val scalalibVersion = "11.7.0"

val commonSettings = Seq(
  scalacOptions := Seq(
    "-encoding",
    "utf-8",
    "-rewrite",
    "-source:3.7-migration",
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
    "org.lichess"   %% "scalalib-core"  % scalalibVersion,
    "org.lichess"   %% "scalalib-model" % scalalibVersion,
    "org.typelevel" %% "cats-core"      % "2.13.0",
    "org.typelevel" %% "alleycats-core" % "2.13.0",
    "org.typelevel" %% "cats-parse"     % "1.1.0",
    "dev.optics"    %% "monocle-core"   % "3.3.0",
    "org.typelevel" %% "kittens"        % "3.5.0"
  ),
  resolvers += "lila-maven".at("https://raw.githubusercontent.com/ornicar/lila-maven/master")
)

lazy val playJson: Project = Project("playJson", file("playJson"))
  .settings(
    commonSettings,
    name := "scalachess-play-json",
    libraryDependencies ++= List(
      "org.playframework" %% "play-json"          % "3.0.4",
      "org.lichess"       %% "scalalib-play-json" % scalalibVersion
    )
  )
  .dependsOn(scalachess)

lazy val rating: Project = Project("rating", file("rating"))
  .settings(
    commonSettings,
    name := "scalachess-rating"
  )
  .dependsOn(scalachess)

lazy val bench = project
  .enablePlugins(JmhPlugin)
  .settings(commonSettings, scalacOptions -= "-Wunused:all", name := "bench")
  .settings(publish := {}, publish / skip := true)
  .disablePlugins(ScalafixPlugin)
  .dependsOn(scalachess, testKit, testKit % "compile->test")

lazy val testKit = project
  .in(file("./test-kit"))
  .settings(
    commonSettings,
    name := "scalachess-test-kit",
    libraryDependencies ++= List(
      "org.scalacheck"      %% "scalacheck"        % "1.18.1",
      "org.typelevel"       %% "literally"         % "1.2.0",
      "org.scalameta"       %% "munit"             % "1.1.1"  % Test,
      "org.scalameta"       %% "munit-scalacheck"  % "1.1.0"  % Test,
      "com.disneystreaming" %% "weaver-cats"       % "0.8.4"  % Test,
      "com.disneystreaming" %% "weaver-scalacheck" % "0.8.4"  % Test,
      "co.fs2"              %% "fs2-core"          % "3.12.0" % Test,
      "co.fs2"              %% "fs2-io"            % "3.12.0" % Test,
      "org.typelevel"       %% "discipline-munit"  % "2.0.0"  % Test,
      "org.typelevel"       %% "cats-laws"         % "2.13.0" % Test
    )
  )
  .dependsOn(scalachess % "compile->compile", rating % "compile->compile")

lazy val root = project
  .in(file("."))
  .settings(publish := {}, publish / skip := true)
  .aggregate(scalachess, rating, playJson, testKit, bench)

addCommandAlias("prepare", "scalafixAll; scalafmtAll")
addCommandAlias("check", "; scalafixAll --check; scalafmtCheckAll")
