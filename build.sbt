lazy val scalachess = Project("scalachess", file("."))
name                                            := "scalachess"
organization                                    := "org.lichess"
version                                         := "11.0.0"
ThisBuild / scalaVersion                        := "3.1.1"
ThisBuild / githubWorkflowPublishTargetBranches := Seq() // Don't publish anywhere
ThisBuild / githubWorkflowBuild ++= Seq(
  WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check Formatting"))
)

libraryDependencies ++= List(
  "org.typelevel"      %% "cats-parse"     % "0.3.7",
  "org.specs2"         %% "specs2-core"    % "4.15.0" % Test,
  "org.specs2"         %% "specs2-cats"    % "4.15.0" % Test,
  "com.github.ornicar" %% "scalalib"       % "8.0.2",
  "joda-time"           % "joda-time"      % "2.10.14",
  "org.typelevel"      %% "cats-core"      % "2.7.0",
  "org.typelevel"      %% "alleycats-core" % "2.7.0"
)

resolvers ++= Seq(
  "lila-maven" at "https://raw.githubusercontent.com/ornicar/lila-maven/master"
)

scalacOptions := Seq(
  "-encoding",
  "utf-8",
  "-rewrite",
  "-source:future-migration",
  "-indent",
  "-explaintypes",
  "-feature",
  "-language:postfixOps"
  // Warnings as errors!
  // "-Xfatal-warnings",
)

publishTo := Option(Resolver.file("file", new File(sys.props.getOrElse("publishTo", ""))))
