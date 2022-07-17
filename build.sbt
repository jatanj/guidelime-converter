val scala2Version = "2.13.8"
val scalac2Options = Seq(
  "-Wunused"
)

Global / onChangedBuildSource := ReloadOnSourceChanges
Global / watchTriggeredMessage := Watch.clearScreenOnTrigger

ThisBuild / organization := "dev.jatan"
ThisBuild / version := s"0.1.0"
ThisBuild / scalacOptions ++= Seq(
  "-language:higherKinds",
  "-language:implicitConversions",
  "-encoding", "utf8",
  "-feature",
  "-unchecked",
  "-deprecation"
)

lazy val guidelimeConverter = (project in file("."))
  .settings(
    name := "guidelime-converter",
    scalaVersion := scala2Version,
    scalacOptions ++= scalac2Options,
    libraryDependencies := Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.3",
      "org.jsoup" % "jsoup" % "1.13.1",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.github.openjson" % "openjson" % "1.0.12",
      "junit" % "junit" % "4.13.1" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "junit-4-13" % "3.2.2.0" % Test,
      "org.scala-lang.modules" %% "scala-xml" % "1.2.0" % Test
    )
  )
