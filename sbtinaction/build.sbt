name := "preowned-kittens"

lazy val checkUname = TaskKey[String]("Check the uname of the system")

checkUname in ThisBuild := Process("uname -a").lines.head

version := "1.0"

def PreownedKittenProject(name: String): Project = {
  Project(name, file(name))
  .settings(
      version      := "1.0",
      organization := "com.preownedkittens",
      libraryDependencies ++= Seq(
        "com.typesafe.scala-logging" % "scala-logging-slf4j_2.11" % "2.1.2",
        "ch.qos.logback" % "logback-classic" % "1.0.7",
        "junit" % "junit" % "4.11" % "test"
      )
    )
}

lazy val common = (
    PreownedKittenProject("common")
      .settings()
  )

lazy val analytics = (
  PreownedKittenProject("analytics")
  .dependsOn(common)
  .settings()
  )

lazy val website = (
  PreownedKittenProject("website")
    .dependsOn(common)
    .settings()
)

includeFilter in (Compile, unmanagedSources) := "*.scala"

excludeFilter in (Compile, unmanagedSources) := NothingFilter

mappings in packageBin in Compile += (baseDirectory.value / "LICENSE") -> "PREOWNED-KITTEN-LICENSE" 
