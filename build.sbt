lazy val scalachess = Project("scalachess", file(".")).settings(
  name := "scalachess",
  libraryDependencies ++= List(
    "org.specs2"         %% "specs2-core"    % "4.19.0" % Test,
    "org.specs2"         %% "specs2-cats"    % "4.19.0" % Test,
    "com.github.ornicar" %% "scalalib"       % "9.0.2",
    "joda-time"           % "joda-time"      % "2.12.2",
    "org.typelevel"      %% "cats-core"      % "2.9.0",
    "org.typelevel"      %% "alleycats-core" % "2.9.0",
    "org.typelevel"      %% "cats-parse"     % "0.3.8"
  ),
  scalacOptions := Seq(
    "-encoding",
    "utf-8",
    // "-rewrite",
    "-source:future-migration",
    "-indent",
    "-explaintypes",
    "-feature",
    "-language:postfixOps"
    // Warnings as errors!
    /* "-Xfatal-warnings" */
  )
)

ThisBuild / organization           := "org.lichess"
ThisBuild / version                := "13.2.5"
ThisBuild / scalaVersion           := "3.2.1"
ThisBuild / licenses += "AGPL-3.0" -> url("https://opensource.org/licenses/AGPL-3.0")

resolvers += "lila-maven" at "https://raw.githubusercontent.com/ornicar/lila-maven/master"

publishTo := Option(Resolver.file("file", new File(sys.props.getOrElse("publishTo", ""))))

lazy val bench = project
  .enablePlugins(JmhPlugin)
  .settings(name := "bench")
  .dependsOn(scalachess, scalachess % "compile->test")
