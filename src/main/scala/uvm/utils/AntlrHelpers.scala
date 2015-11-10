package uvm.utils

import org.antlr.v4.runtime.RecognitionException
import scala.collection.mutable.ArrayBuffer
import org.antlr.v4.runtime.Recognizer
import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.Token

object AntlrHelpers {
  class AccumulativeAntlrErrorListener(source: String) extends BaseErrorListener {
    val buf = new ArrayBuffer[String]()
    var hasError = false

    lazy val sourceLines = ArrayBuffer(source.lines.toSeq: _*)

    override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: Object,
      line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
      val theLine = sourceLines(line - 1)
      val marker = " " * charPositionInLine + "^"
      buf += "line %d:%d %s\n%s\n%s".format(line, charPositionInLine, msg, theLine, marker)
      hasError = true
    }

    def getMessages(): String = buf.mkString("\n")
  }

  def inCtx(ctx: ParserRuleContext, s: String): String = nearTok(ctx.getStart, s)
  def inCtx(ctx: TerminalNode, s: String): String = nearTok(ctx.getSymbol, s)

  def nearTok(tok: Token, s: String): String = {
    val line = tok.getLine()
    val column = tok.getCharPositionInLine()
    val near = tok.getText()
    return "At %d:%d near '%s': %s".format(line, column, near, s)
  }
}

trait AdvancedAntlrHelper {
  def sourceLines: IndexedSeq[String]

  def inCtx(ctx: ParserRuleContext, s: String): String = {
    val tok1 = ctx.getStart
    val tok2 = ctx.getStop
    val firstLine = nearTok(tok1, s)

    val line = tok1.getLine()
    val theLine = sourceLines(line - 1)

    val line2 = tok2.getLine()
    val column = tok1.getCharPositionInLine()
    val end = if (line == line2) tok2.getCharPositionInLine else theLine.length()

    val near = tok1.getText()

    val marker = " " * column + "^" + "~" * (end - column)
    "At %d:%d near '%s': %s\n%s\n%s".format(line, column, near, s, theLine, marker)
  }

  def inCtx(ctx: TerminalNode, s: String): String = nearTok(ctx.getSymbol, s)

  def nearTok(tok: Token, s: String): String = {
    val line = tok.getLine()
    val column = tok.getCharPositionInLine()
    val near = tok.getText()
    return "At %d:%d near '%s': %s".format(line, column, near, s)
  }
}