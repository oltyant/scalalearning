import sbt.Keys._

name := "Scala Learning"

version := "1.0"

scalaVersion := "2.11.5"

crossScalaVersions := Seq("2.11.5")

def ScalaLearningProject(name: String): Project = {
  Project(name, file(name))
    .settings(
      scalacOptions += "-target:jvm-1.7",
      scalaVersion := "2.11.5",
      crossScalaVersions := Seq("2.11.5", "2.10.4"),
      version      := "1.0",
      organization := "com.scalalearning",
      resolvers ++= Seq(
        "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
        "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"
      ),
      libraryDependencies ++= Seq(
        "org.specs2" %% "specs2" % "2.4.17" % "test",
        "org.scalatest" %% "scalatest" % "2.2.4" % "test",
        "org.scalacheck" %% "scalacheck" % "latest.release" % Test
      ),
      resolvers += Classpaths.sbtPluginReleases,
      ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) },
      (scalastyleFailOnError in Global) := true
  ).enablePlugins(CrossPerProjectPlugin)
}

lazy val fpinscala = (
  ScalaLearningProject("fpinscala")
    .settings(
      scalaVersion := "2.11.5"
    )
)

lazy val fpinscala_coursera = (
  ScalaLearningProject("fpinscala-coursera")
    .dependsOn(fpinscala)
    .settings()
)

lazy val reactivescala_coursera = (
  ScalaLearningProject("reactivescala-coursera")
    .settings(
      crossScalaVersions := Seq("2.11.5"),
      libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "latest.release" % Compile,
        //"com.netflix.rxjava" % "rxjava-scala" % "0.15.0",
        "org.json4s" %% "json4s-native" % "3.2.11",
        "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "org.slf4j" % "slf4j-api" % "1.7.5",
        "org.slf4j" % "slf4j-simple" % "1.7.5",
        "com.squareup.retrofit" % "retrofit" % "1.0.0",
        "org.scala-lang.modules" %% "scala-async" % "0.9.2",
        "io.reactivex" %% "rxscala" % "0.23.0",
        "io.reactivex" % "rxswing" % "0.21.0",
        "org.scala-lang.modules" % "scala-swing_2.11.0-RC4" % "1.0.1",
        "com.typesafe.akka" % "akka-actor_2.11" % "2.3.9",
        "com.typesafe.akka" % "akka-testkit_2.11" % "2.3.9",
        "com.typesafe.akka" % "akka-persistence-experimental_2.11" % "2.3.9"
      )
    )
)

lazy val impatient = (
  ScalaLearningProject("impatient")
    .dependsOn(fpinscala)
    .settings(
      version := "0.1"
    )
)

lazy val mallonscala = (
  ScalaLearningProject("otherscala")
    .dependsOn(fpinscala)
    .settings()
)

lazy val fpoverjvm = (
  ScalaLearningProject("fpoverjvm")
    .dependsOn(fpinscala)
    .settings()
)

lazy val neophyte = (
  ScalaLearningProject("neophytes-guide")
    .dependsOn(fpinscala)
    .settings()
)

lazy val sbtinaction = (
  ScalaLearningProject("sbtinaction")
    .dependsOn(fpinscala)
    .settings()
)

lazy val akkainaction = (
  ScalaLearningProject("akkainaction")
    .dependsOn(fpinscala)
    .settings()
)

lazy val scalaz = (
  ScalaLearningProject("scalaz")
    .dependsOn(fpinscala)
    .settings()
)
