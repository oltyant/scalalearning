name := "Scala Learning"

version := "1.0"

scalaVersion := "2.11.5"

crossScalaVersions := Seq("2.10.4", "2.11.5") 

def ScalaLearningProject(name: String): Project = {
  Project(name, file(name))
    .settings(
      version      := "1.0",
      organization := "com.scalalearning",
      libraryDependencies ++= Seq(
        "org.specs2" % "specs2_2.11" % "3.0-M2-scalaz-7.0.6" % "test"
      ),
      resolvers += Classpaths.sbtPluginReleases,
      ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }
  )
}

lazy val fpinscala = (
  ScalaLearningProject("fpinscala")
    .settings()
)

lazy val fpinscala_coursera = (
  ScalaLearningProject("fpinscala-coursera")
    .dependsOn(fpinscala)
    .settings()
)

lazy val reactivescala_coursera = (
  ScalaLearningProject("reactivescala-coursera")
    .dependsOn(fpinscala)
    .settings()
)

lazy val impatient = (
  ScalaLearningProject("impatient")
    .dependsOn(fpinscala)
    .settings(
      version := "0.1",
      libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "2.2.4" % "test",
        "org.scalacheck" %% "scalacheck" % "1.12.2"
      )
    )
)

lazy val mallonscala = (
  ScalaLearningProject("mallonscala")
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
