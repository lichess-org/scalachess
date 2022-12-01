lazy val scalachess = Project("scalachess", file("."))
name                   := "scalachess"
organization           := "org.lichess"
version                := "13.0.0"
scalaVersion           := "3.2.1"
licenses += "AGPL-3.0" -> url("https://opensource.org/licenses/AGPL-3.0")

libraryDependencies ++= List(
  "org.specs2"         %% "specs2-core"    % "4.17.0" % Test,
  "org.specs2"         %% "specs2-cats"    % "4.17.0" % Test,
  "com.github.ornicar" %% "scalalib"       % "9.0.1",
  "joda-time"           % "joda-time"      % "2.12.1",
  "org.typelevel"      %% "cats-core"      % "2.9.0",
  "org.typelevel"      %% "alleycats-core" % "2.9.0",
  "org.typelevel"      %% "cats-parse"     % "0.3.8"
)

resolvers += "lila-maven" at "https://raw.githubusercontent.com/ornicar/lila-maven/master"

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

publishTo := Option(Resolver.file("file", new File(sys.props.getOrElse("publishTo", ""))))
