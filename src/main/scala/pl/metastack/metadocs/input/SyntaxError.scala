package pl.metastack.metadocs.input

import fastparse.core.Result

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
  def fromFarseParse(f: Result.Failure): SyntaxError = {
    val frames = f.traced.fullStack.map { frame =>
      val lines = f.input.take(frame.index + 1).lines.toVector
      val line = lines.length
      val column = lines.last.length
      StackFrame(line, column, frame.parser.toString)
    }

    SyntaxError(f.input, f.traced.expected, f.line, f.col, frames)
  }
}
