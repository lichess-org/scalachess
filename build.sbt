inThisBuild(
  Seq(
    scalaVersion       := "3.4.1",
    version            := "15.10.1",
    organization       := "org.lichess",
    licenses += ("MIT" -> url("https://opensource.org/licenses/MIT")),
    publishTo          := Option(Resolver.file("file", new File(sys.props.getOrElse("publishTo", "")))),
    semanticdbEnabled  := true // for scalafix
  )
)

val scalalibVersion = "11.1.0"

val commonSettings = Seq(
  scalacOptions := Seq(
    "-encoding",
    "utf-8",
    "-rewrite",
    "-source:3.4-migration",
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
    "org.typelevel" %% "cats-core"      % "2.10.0",
    "org.typelevel" %% "alleycats-core" % "2.10.0",
    "org.typelevel" %% "cats-parse"     % "1.0.0",
    "dev.optics"    %% "monocle-core"   % "3.2.0",
    "org.typelevel" %% "kittens"        % "3.3.0"
  ),
  resolvers += "lila-maven".at("https://raw.githubusercontent.com/ornicar/lila-maven/master")
)

lazy val playJson: Project = Project("playJson", file("playJson"))
  .settings(
    commonSettings,
    name := "scalachess-play-json",
    libraryDependencies ++= List(
      "org.playframework" %% "play-json"          % "3.0.2",
      "org.lichess"       %% "scalalib-play-json" % scalalibVersion
    )
  )
  .dependsOn(scalachess)

lazy val bench = project
  .enablePlugins(JmhPlugin)
  .settings(commonSettings, scalacOptions -= "-Wunused:all", name := "bench")
  .dependsOn(scalachess, testKit, testKit % "compile->test")

lazy val testKit = project
  .in(file("./test-kit"))
  .enablePlugins(JmhPlugin)
  .settings(
    commonSettings,
    name := "scalachess-test-kit",
    libraryDependencies ++= List(
      "org.scalacheck"      %% "scalacheck"        % "1.17.0",
      "org.scalameta"       %% "munit"             % "1.0.0-M11" % Test,
      "org.scalameta"       %% "munit-scalacheck"  % "1.0.0-M11" % Test,
      "com.disneystreaming" %% "weaver-cats"       % "0.8.3"     % Test,
      "com.disneystreaming" %% "weaver-scalacheck" % "0.8.3"     % Test,
      "co.fs2"              %% "fs2-core"          % "3.8.0"     % Test,
      "co.fs2"              %% "fs2-io"            % "3.8.0"     % Test,
      "org.typelevel"       %% "discipline-munit"  % "1.0.9"     % Test,
      "org.typelevel"       %% "cats-laws"         % "2.9.0"     % Test
    )
  )
  .dependsOn(scalachess % "compile->compile")

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .aggregate(scalachess, playJson, testKit)

addCommandAlias("fmtCheck", "all scalachess/scalafmtCheckAll bench/scalafmtCheckAll testKit/scalafmtCheckAll")
addCommandAlias("fmt", "all scalachess/scalafmtAll bench/scalafmtAll testKit/scalafmtAll")

addCommandAlias("scalafixCheck", "; scalafixAll --check ; testKit/scalafixAll --check")
addCommandAlias("scalafixCheck", "; scalafixAll --check ; testKit/scalafixAll --check")

addCommandAlias("prepare", "scalafixAll; testKit/scalafixAll; fmt")
addCommandAlias("check", "; scalafixCheck; fmtCheck")
