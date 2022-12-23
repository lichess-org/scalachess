lazy val scalachess = Project("scalachess", file("."))
name                   := "scalachess"
organization           := "org.lichess"
version                := "13.2.2"
scalaVersion           := "3.2.1"
licenses += "AGPL-3.0" -> url("https://opensource.org/licenses/AGPL-3.0")

libraryDependencies ++= List(
  "org.specs2"         %% "specs2-core"      % "4.19.0" % Test,
  "org.specs2"         %% "specs2-cats"      % "4.19.0" % Test,
  "org.scalameta"      %% "munit"            % "1.0.0-M7" % Test,
  "org.scalacheck"     %% "scalacheck"       % "1.16.0" % Test,
  "org.scalameta"      %% "munit-scalacheck" % "1.0.0-M7" % Test,
  "com.github.lenguyenthanh" % "compression"      % "aacf55bea2" % Test, // a fork of lichess compression which public everything so we can use it for testing.
  "com.github.ornicar" %% "scalalib"         % "9.0.2",
  "joda-time"           % "joda-time"        % "2.12.2",
  "org.typelevel"      %% "cats-core"        % "2.9.0",
  "org.typelevel"      %% "alleycats-core"   % "2.9.0",
  "org.typelevel"      %% "cats-parse"       % "0.3.8"
)

resolvers += "lila-maven" at "https://raw.githubusercontent.com/ornicar/lila-maven/master"
resolvers += "jitpack" at "https://jitpack.io"

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
