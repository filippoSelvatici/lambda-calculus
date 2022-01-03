Global / onChangedBuildSource := ReloadOnSourceChanges

val projectName = "lambda-calculus"

lazy val root = (project in file("."))
  .settings(
    name := projectName,
    scalaVersion := "3.1.0",

    scalacOptions ++= Seq(
        "-deprecation",
        "-feature",
        "-unchecked"
    ), 
    libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest-funspec" % "3.2.10" % Test,
    )
  )