import sbt.Keys._

name := "Scala Learning"

version := "1.0"

scalaVersion in ThisBuild := "2.12.7"

autoScalaLibrary := false

lazy val  commonSettings = Seq(
  scalaVersion := "2.12.7",
  scalacOptions ++= Seq("-target:jvm-1.8", "-Yrangepos"),
  dependencyOverrides ++= Set(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.scala-lang" % "scala-library" % scalaVersion.value,
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang" % "scalap" % scalaVersion.value,
    "org.specs2" %% "specs2-core" % "4.3.4" % "test",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test",
    "org.scalacheck" %% "scalacheck" % "latest.release" % Test
  )
)


def ScalaLearningProject(name: String): Project = {
  Project(name, file(name))
    .settings(
      commonSettings,
      version      := "1.0",
      organization := "com.scalalearning",
      resolvers ++= Seq(
        "scoverage-bintray" at "https://dl.bintray.com/sksamuel/sbt-plugins/",
        "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
        "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"
      ),
      libraryDependencies ++= Seq(
        "junit" % "junit" % "4.11" % Test,
        "org.specs2" %% "specs2-core" % "4.3.4" % "test",
        "org.scalatest" %% "scalatest" % "3.0.5" % "test",
        "org.scalacheck" %% "scalacheck" % "latest.release" % Test
      ),
      resolvers += Classpaths.sbtPluginReleases,
      ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }
  ).enablePlugins(CrossPerProjectPlugin)
}

lazy val fpinscala = (
  ScalaLearningProject("fpinscala")
    .settings(commonSettings)
)

lazy val fpinscala_coursera = (
  ScalaLearningProject("fpinscala-coursera")
    .dependsOn(fpinscala)
    .settings(commonSettings)
)

lazy val reactivescala_coursera = (
  ScalaLearningProject("reactivescala-coursera")
    .settings(
      commonSettings,
      libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "latest.release" % Compile,
        //"com.netflix.rxjava" % "rxjava-scala" % "0.15.0",
        "org.json4s" %% "json4s-native" % "3.6.2",
        "net.databinder.dispatch" %% "dispatch-core" % "0.13.4",
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "org.slf4j" % "slf4j-api" % "1.7.25",
        "org.slf4j" % "slf4j-simple" % "1.7.25",
        "com.squareup.retrofit" % "retrofit" % "1.9.0",
        "org.scala-lang.modules" %% "scala-async" % "0.9.7",
        "io.reactivex" %% "rxscala" % "0.26.5",
        "io.reactivex" % "rxswing" % "0.27.0",
        "org.scala-lang.modules" %% "scala-swing" % "2.0.3",
        "com.typesafe.akka" %% "akka-actor" % "2.5.18",
        "com.typesafe.akka" %% "akka-testkit" % "2.5.18",
        "com.typesafe.akka" %% "akka-persistence" % "2.5.18"
      )
    )
)

lazy val impatient = (
  ScalaLearningProject("impatient")
    .dependsOn(fpinscala)
    .settings(
      version := "0.1",
      commonSettings
    )
)

lazy val mallonscala = (
  ScalaLearningProject("otherscala")
    .dependsOn(fpinscala)
    .settings(commonSettings)
)

lazy val fpoverjvm = (
  ScalaLearningProject("fpoverjvm")
    .dependsOn(fpinscala)
    .settings(commonSettings)
)

lazy val neophyte = (
  ScalaLearningProject("neophytes-guide")
    .dependsOn(fpinscala)
    .settings(commonSettings)
)

lazy val sbtinaction = (
  ScalaLearningProject("sbtinaction")
    .dependsOn(fpinscala)
    .settings(commonSettings)
)

lazy val akkainaction = (
  ScalaLearningProject("akkainaction")
    .dependsOn(fpinscala)
    .settings(commonSettings)
)

lazy val scalaz = (
  ScalaLearningProject("scalaz")
    .dependsOn(fpinscala)
    .settings(commonSettings)
)

lazy val cats = (
  ScalaLearningProject("cats")
      .settings(
        commonSettings,
        libraryDependencies ++= Seq(
          "org.typelevel" %% "cats" % "0.9.0",
          "org.scalatest" %% "scalatest" % "3.0.5" % Test
        )
      )
)
