name := "common"

//lazy val printTestFrameworks = TaskKey[Unit]("Print the current test frameworks")

//printTestFrameworks in ThisBuild := println(TestFrameworks)

testOptions in Test ++= Seq(
							Tests.Argument(TestFrameworks.Specs2, "html"),
							Tests.Argument(TestFrameworks.JUnit, "--run-listener=org.preownedkittens.sbt.JUnitListener"),
							Tests.Argument(TestFrameworks.ScalaCheck, "-s", "5000")
						)

javaOptions in Test ++= Seq(
							"-Dspecs2.outDir=" + (target.value / "generated/test-reports").getAbsolutePath,
							"-Djunit.output.file=" + (target.value / "generated/junit.html").getAbsolutePath
					)

fork in Test := true 