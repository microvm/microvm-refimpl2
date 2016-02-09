package uvm.clientsupport.text

import java.util.regex.Pattern

import org.scalatest.matchers.{MatchResult, Matcher}

trait TextOutputMatchers {
  private val varPattern = Pattern compile "[@%]\\$(\\w+)"
  private val specialPattern = Pattern compile "\\s+|[@%]\\$\\w+"

  class IRTemplateMatcher(outputTemplate: String) extends Matcher[BundleBuilder] {
    private val lines = outputTemplate split "\n" map (_.trim) filter (_.nonEmpty)
    private lazy val varIndexes = lines map { line =>
      val matcher = varPattern matcher line
      var indexes = Seq.empty[String]
      while (matcher.find) indexes :+= matcher group 1
      indexes
    }
    private lazy val linePatterns = lines map (line => Pattern compile {
      var pattern = "^"
      var lastEnd = 0
      val matcher = specialPattern matcher line
      while (matcher.find) {
        val slice = line.substring(lastEnd, matcher.start)
        if (slice.nonEmpty) pattern += Pattern quote slice
        if (matcher.group matches "\\s+") pattern += "\\s+"
        else pattern += "([@%][\\w\\-.]+)"
        lastEnd = matcher.end
      }
      if (lastEnd < line.length) pattern += (Pattern quote (line substring lastEnd))
      pattern + "$"
    })
    override def apply(left: BundleBuilder): MatchResult = {
      val ir = left.build().toString
      val testLines = ir split "\n" map (_.trim) filter (_.nonEmpty)
      var varMap = Map.empty[String, String]
      for ((((line, n), pattern), vars) <- testLines.zipWithIndex zip linePatterns zip varIndexes) {
        val matcher = pattern matcher line
        if (matcher.matches) {
          for ((varName, index) <- vars.zipWithIndex) {
            varMap get varName match {
              case Some(value) =>
                val newValue = matcher group (index+1)
                if (newValue != value) return MatchResult(matches = false,
                  s"Placeholder variable $varName had differing values ($value, $newValue)\nin IR:\n$ir",
                  s"Placeholder variable $varName had consistent value ($value)")
              case None => varMap += varName -> (matcher group (index+1))
            }
          }
        } else return MatchResult(matches = false,
          s"""Line $n, "$line", did not match regex "$pattern"
in IR:
$ir""",
          s"""Line $n, "$line", matched regex "$pattern"""")
      }
      MatchResult(lines.length == testLines.length,
        s"IR with ${testLines.length} lines did not match template with ${lines.length} lines",
        s"IR with ${testLines.length} lines matched template with ${lines.length} lines")
    }
  }

  def matchIRTemplate(template: String) = new IRTemplateMatcher(template)
}

object TextOutputMatchers extends TextOutputMatchers
