package pl.metastack.metadocs.input

import fastparse.core.Parsed.Failure

case class StackFrame(position: String, rule: String) {
  override def toString = s"$rule ($position)"
}

case class SyntaxError(literal: String,
                       expected: String,
                       position: String,
                       locationCode: String,
                       frames: Seq[StackFrame]) {
  override def toString: String =
    s"found $literal, expected $expected at position $position\n" +
    locationCode + "\n" +
    "Frames:\n" +
    frames.map("  " + _.toString).mkString("\n")
}

object SyntaxError {
  def fromFarseParse(f: Failure[Char, String]): SyntaxError = {
    val ctx = f.extra
    val frames = ctx.traced.fullStack.map { frame =>
      val position = ctx.input.repr.prettyIndex(ctx.input, frame.index)
      StackFrame(position, frame.parser.toString)
    }

    // Taken from fastparse/StringReprOps
    val literal = ctx.input.repr.literalize(ctx.input.slice(ctx.traced.index, ctx.traced.index + 20))
    val position = ctx.input.repr.prettyIndex(ctx.input, f.index)
    val locationCode = {
      val first = ctx.input.slice(ctx.traced.index - 20, ctx.traced.index)
      val last = ctx.input.slice(ctx.traced.index, ctx.traced.index + 20)
      val emptyString = ""
      val lastSnippet = last.lines.toSeq.headOption.getOrElse(emptyString)
      val firstSnippet = first.reverse.lines.toSeq.headOption.getOrElse(emptyString).reverse

      ctx.input.repr.prettyPrint(firstSnippet) + ctx.input.repr.prettyPrint(lastSnippet) + "\n" + (" " * firstSnippet.length) + "^"
    }

    SyntaxError(literal, ctx.traced.expected, position, locationCode, frames)
  }
}
