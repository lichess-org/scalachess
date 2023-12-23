ThisBuild / organization      := "org.lichess"
ThisBuild / version           := "15.7.3"
ThisBuild / scalaVersion      := "3.3.1"
ThisBuild / licenses += "MIT" -> url("https://opensource.org/licenses/MIT")

ThisBuild / resolvers += "lila-maven" at "https://raw.githubusercontent.com/ornicar/lila-maven/master"
ThisBuild / publishTo := Option(Resolver.file("file", new File(sys.props.getOrElse("publishTo", ""))))

val commonSettings = Seq(
  scalacOptions := Seq(
    "-encoding",
    "utf-8",
    "-source:future-migration",
    "-indent",
    "-feature",
    "-language:postfixOps",
    "-Wunused:all",
    "-release:21"
    // "-Werror"
    // Warnings as errors!
    /* "-Xfatal-warnings" */
  )
)

lazy val scalachess: Project = Project("scalachess", file(".")).settings(
  commonSettings,
  name := "scalachess",
  libraryDependencies ++= List(
    "com.github.ornicar" %% "scalalib"       % "9.5.5",
    "org.typelevel"      %% "cats-core"      % "2.10.0",
    "org.typelevel"      %% "alleycats-core" % "2.10.0",
    "org.typelevel"      %% "cats-parse"     % "1.0.0",
    "dev.optics"         %% "monocle-core"   % "3.2.0",
    "org.typelevel"      %% "kittens"        % "3.1.0"
  )
)

lazy val bench = project
  .enablePlugins(JmhPlugin)
  .settings(commonSettings, name := "bench")
  .dependsOn(scalachess, testKit, testKit % "compile->test")

lazy val testKit = project
  .in(file("./test-kit"))
  .enablePlugins(JmhPlugin)
  .settings(
    commonSettings,
    name := "scalachess-test-kit",
    libraryDependencies ++= List(
      "org.scalacheck"      %% "scalacheck"        % "1.17.0",
      "org.scalameta"       %% "munit"             % "1.0.0-M8" % Test,
      "org.scalameta"       %% "munit-scalacheck"  % "1.0.0-M8" % Test,
      "com.disneystreaming" %% "weaver-cats"       % "0.8.3"    % Test,
      "com.disneystreaming" %% "weaver-scalacheck" % "0.8.3"    % Test,
      "co.fs2"              %% "fs2-core"          % "3.8.0"    % Test,
      "co.fs2"              %% "fs2-io"            % "3.8.0"    % Test,
      "org.typelevel"       %% "discipline-munit"  % "1.0.9"    % Test,
      "org.typelevel"       %% "cats-laws"         % "2.9.0"    % Test
    ),
    testFrameworks += new TestFramework("weaver.framework.CatsEffect")
  )
  .dependsOn(scalachess % "compile->compile")
