name := "scalachess"

organization := "org.lichess"

version := "9.3.2"

scalaVersion := "2.13.3"

libraryDependencies ++= List(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalaz"             %% "scalaz-core"              % "7.2.30",
  "org.specs2"             %% "specs2-core"              % "4.7.0" % "test",
  "org.specs2"             %% "specs2-scalaz"            % "4.7.0" % "test",
  "com.github.ornicar"     %% "scalalib"                 % "6.8",
  "joda-time"              % "joda-time"                 % "2.10.6"
)

resolvers ++= Seq(
  "lila-maven" at "https://raw.githubusercontent.com/ornicar/lila-maven/master",
  "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases"
)

scalacOptions ++= Seq(
    "-encoding",
    "utf-8",
    "-explaintypes",
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-Ymacro-annotations",
    // Warnings as errors!
    "-Xfatal-warnings",
    // Linting options
    "-unchecked",
    "-Xcheckinit",
    "-Xlint:adapted-args",
    "-Xlint:constant",
    "-Xlint:delayedinit-select",
    "-Xlint:deprecation",
    "-Xlint:inaccessible",
    "-Xlint:infer-any",
    "-Xlint:missing-interpolator",
    "-Xlint:nullary-unit",
    "-Xlint:option-implicit",
    "-Xlint:package-object-classes",
    "-Xlint:poly-implicit-overload",
    "-Xlint:private-shadow",
    "-Xlint:stars-align",
    "-Xlint:type-parameter-shadow",
    "-Wdead-code",
    "-Wextra-implicit",
    // "-Wnumeric-widen",
    "-Wunused:imports",
    "-Wunused:locals",
    "-Wunused:patvars"
    // "-Wunused:privates", // unfortunately doesn't work with macros
    // "-Wunused:implicits",
    // "-Wunused:params",
    // "-Wvalue-discard",
)

publishTo := Some(Resolver.file("file", new File(sys.props.getOrElse("publishTo", ""))))
