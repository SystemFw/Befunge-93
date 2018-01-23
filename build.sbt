lazy val root = (project in file(".")).settings(
  commonSettings,
  consoleSettings,
  compilerOptions,
  typeSystemEnhancements,
  dependencies
)

lazy val commonSettings = Seq(
  name := "Befunge-93",
  scalaVersion in ThisBuild := "2.11.11",
  crossScalaVersions := Seq("2.11.11", "2.12.1")
)

lazy val consoleSettings = Seq(
  initialCommands := s"import befunge._",
  scalacOptions in (Compile, console) -= "-Ywarn-unused-import"
)

lazy val compilerOptions =
  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-encoding",
    "utf8",
    "-target:jvm-1.8",
    "-feature",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-Ypartial-unification",
    "-Ywarn-unused-import",
    "-Ywarn-value-discard"
  )

lazy val typeSystemEnhancements =
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

def dep(org: String)(version: String)(modules: String*) =
  Seq(modules: _*) map { name =>
    org %% name % version
  }

lazy val dependencies = {
  val cats = dep("org.typelevel")("1.0.1")(
    "cats-core",
    "cats-macros",
    "cats-kernel",
    "cats-free"
  )

  val fs2 = dep("co.fs2")("0.10.0-M11")(
    "fs2-core",
    "fs2-io"
  )

  def extraResolvers =
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    )

  val deps =
    libraryDependencies ++= Seq(
      cats,
      fs2
    ).flatten

  Seq(deps, extraResolvers)
}
