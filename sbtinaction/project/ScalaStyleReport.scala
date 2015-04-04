import java.io._
import java.util
import java.util.{HashMap => JHashMap}

import scala.xml._
import scala.collection.convert.WrapAsJava._
import org.apache.velocity.VelocityContext
import org.apache.velocity.app.Velocity
import java.util.{Collection => JCollection}

case class CodeLine(className: String, text: String)

case class ScalastyleError(name: String, line: String, level: String, message: String, codeLines: JCollection[CodeLine])

object ScalastyleReport {
  val lineNumberDiff = 5

  def report(outputDir: File, outputFile: String, templateFile: File,
             reportXml: File): File = {
    def attr(node: Node, name: String) = (node \\ ("@" + name)).text

    val xml = XML.loadFile(reportXml)

    // get scalastyle errors from XML using the Scala built-in XML processing
    val errors = asJavaCollection((xml \\ "checkstyle" \\ "file").map(f => {
      val name = attr(f, "name")
      (f \\ "error").map { e =>
        val line = attr(e, "line")
        val severity = attr(e, "severity")
        val message = attr(e, "message")
        val codeLines = gatherCodeLines(name, line.toInt, severity)
        ScalastyleError(name, line, severity, message, codeLines)
      }
    }).flatten)
    sbt.IO.createDirectory(outputDir)
    val context = new JHashMap[String, Any]()
    context.put("results", errors)
    val sw = new StringWriter()
    val template = sbt.IO.read(templateFile)
    Velocity.evaluate(new VelocityContext(context), sw, "velocity", template)
    val reportFile = new File(outputDir, outputFile)
    sbt.IO.write(reportFile, sw.toString())
    reportFile
  }

  private def gatherCodeLines(fileName: String, lineNumber: Int, severity: String): JCollection[CodeLine] = {
    val allLines = sbt.IO.readLines(new File(fileName)).zipWithIndex.map{
      entry => CodeLine(
        if (entry._1 + 1 == lineNumber) severity else "codeLine",
        s"${entry._1 + 1}: ${entry._2}"
      )
    }.toList
    val linesAroundError = asJavaCollection(allLines.slice(
      if (lineNumber <= lineNumberDiff) 0 else lineNumber - lineNumberDiff,
      if (allLines.size - lineNumber > lineNumberDiff) lineNumber + lineNumberDiff else allLines.size
    ))
    linesAroundError
  }
}