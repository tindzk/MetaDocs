package pl.metastack.metadocs.input.markdown

import scala.collection.mutable
import scala.util.Try

import fastparse.all._

import pl.metastack.metadocs.input.metadocs.Parser
import pl.metastack.metadocs.input.metadocs.tree

/** Markdown extension allowing to specify blocks such as:
  * [scala type="imports" name="GettingStartedDatastore"]
  */
object BlockParser {
  val controlChars = Set('#', '=', '[', ']', '"')
  val identifier = P(CharsWhile(
    c => !c.isWhitespace && !controlChars.contains(c)
  ).!)
  val whitespaces = P(CharsWhile(_.isWhitespace).?)
  val stringChars = P(CharsWhile(!Set('\\', '\"').contains(_)))
  val quotedString = P("\"" ~! (stringChars | "\\\"").rep.! ~ "\"")
    .map(_.replaceAllLiterally("\\\"", "\""))
  val argument = P(identifier ~ "=" ~ quotedString).map(tree.Argument.Named.tupled)

  val tag = P(
    "[" ~
      identifier ~
      whitespaces ~
      argument.rep(sep=CharsWhile(_.isWhitespace) ~!).? ~
      whitespaces ~
    "]" ~
      !"(" ~ // Don't match Markdown links
      ("{" ~ Parser.nodes ~ "}").?
  ).map { case (ident, args, children) =>
    tree.Tag(ident, args.getOrElse(Seq.empty), children.getOrElse(Seq.empty))
  }

  def parse(input: String): Try[tree.Tag] = Try(tag.parse(input).get.value)

  def replace(input: String): (String, Seq[tree.Tag]) = {
    var replaced = ""
    val collected = mutable.ArrayBuffer.empty[tree.Tag]
    var inCodeBlock = false

    // Ignore code blocks and verbatim expressions
    def split(s: String) =
      s.replaceAll("`+", "_$0_")
       .split("_")

    split(input).foreach { input =>
      if (Set("```", "``", "`").contains(input)) {
        inCodeBlock = !inCodeBlock
        replaced += input
      } else if (inCodeBlock) {
        replaced += input
      } else {
        var i = 0
        while (i < input.length) {
          if (input(i) != '[') replaced += input(i)
          else {
            val parsed = tag.parse(input.substring(i))
            if (parsed.isInstanceOf[Result.Failure]) replaced += input(i)
            else {
              collected += parsed.get.value
              replaced += "%" + collected.length
              i += parsed.get.index - 1
            }
          }

          i += 1
        }
      }
    }

    (replaced, collected)
  }
}
