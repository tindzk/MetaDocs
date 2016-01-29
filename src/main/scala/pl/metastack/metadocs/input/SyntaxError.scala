package pl.metastack.metadocs.input

import fastparse.core.Parsed.Failure

case class StackFrame(line: Int, column: Int, rule: String) {
  override def toString = s"$rule (line $line, column $column)"
}

case class SyntaxError(input: String,
                       expected: String,
                       line: Int,
                       column: Int,
                       frames: Seq[StackFrame]) {
  override def toString =
    s"Syntax error on line $line, column $column\n" +
    s"  Given: ${input.lines.toSeq(line - 1)}\n" +
     "         " + "".padTo(column - 1, ' ') + "^\n" +
    s"  Expected: $expected\n" +
    "  Frames:\n" +
    frames.map("    " + _.toString).mkString("\n")
}

object SyntaxError {
  def fromFarseParse(f: Failure): SyntaxError = {
    val ctx = f.extra
    val frames = ctx.traced.fullStack.map { frame =>
      val lines = ctx.input.take(frame.index + 1).lines.toVector
      val line = lines.length
      val column = lines.last.length
      StackFrame(line, column, frame.parser.toString)
    }

    SyntaxError(ctx.input, ctx.traced.expected, ctx.line, ctx.col, frames)
  }
}
