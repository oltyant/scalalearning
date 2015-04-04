name := "sbtinaction"

version := "1.0"

//TODO make it inputKey with three possible args css:path xslt:path js:path (the order doesn't matter)
//TODO wrap around column
lazy val scalastyleReport = taskKey[File]("Generate an html report from the xml report of the scalastyle plugin")

scalastyleReport := {
  val xmlReport =  org.scalastyle.sbt.ScalastylePlugin.scalastyle.in(Compile).toTask("").value
  val htmlFile = ScalastyleReport.report(target.value / "html-test-report",
    "scalastyle-report.html",
    baseDirectory.value / "project/scalastyle-report.html",
    target.value / "scalastyle-result.xml")
  println("created report " + htmlFile.getAbsolutePath)
  htmlFile
}

lazy val checkUname = taskKey[String]("Check the uname of the system")

val dependentJarDirectory = settingKey[File]("location of the unpacked dependent jars")

dependentJarDirectory := target.value / "dependent-jars"

val createDependentJarLibrary = taskKey[String]("Check and create the dependent jar library")

checkUname in ThisBuild := Process("uname -a").lines.head

createDependentJarLibrary := {
  sbt.IO.createDirectory(dependentJarDirectory.value)
  dependentJarDirectory.value.getAbsolutePath
}

val excludes = List("meta-inf", "license", "play.plugins", "reference.conf")

def unpackFilter(target: File) = new NameFilter {
  override def accept(name: String): Boolean = !excludes.exists(x => name.toLowerCase.startsWith(x)) && ! file(target.getAbsolutePath() + "/" + name).exists
}

def unpack(target: File, f: File) = {
  if (f.isDirectory) sbt.IO.copyDirectory(f, target)
  else sbt.IO.unzip(f, target, unpackFilter(target))
}

val unpackJars = taskKey[Seq[_]]("unpack all the dependent Jar files into a dependentJarDirectory")

unpackJars := {createDependentJarLibrary.value; Build.data((dependencyClasspath in Runtime).value).map(f => unpack(dependentJarDirectory.value, f))}

val createUberJar = taskKey[File]("create jar which we will run")

createUberJar := {
  def create(jarDir: File, buildJar: File) = {
    val files = (jarDir ** "*").get.filter(_ != jarDir)
    val filesWithPath = files.map(x => (x, x.relativeTo(jarDir).get.getPath))
    sbt.IO.zip(filesWithPath, buildJar)
  }
  create (dependentJarDirectory.value, target.value / "build.jar");
  target.value / "build.jar"
}

def PreownedKittenProject(name: String): Project = {
  Project(name, file(name))
  .settings( Defaults.itSettings : _*)
  .settings(
      version      := "1.0",
      organization := "com.preownedkittens",
      resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
      libraryDependencies ++= Seq(
        "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
        "ch.qos.logback" % "logback-classic" % "1.0.7",
        "junit" % "junit" % "4.11" % "test",
        "org.scalacheck" %% "scalacheck" % "1.12.2",
        "org.specs2" %% "specs2" % "2.4.17" % "test",
        "org.pegdown" % "pegdown" % "1.0.2" % "test",
        "org.scalatest" %% "scalatest" % "2.2.4" % "test,it",
        "org.seleniumhq.selenium" % "selenium-java" % "2.31.0" % "it",
        "com.novocode" % "junit-interface" % "0.11" % "test"
      )
  )
  .configs(IntegrationTest)
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
