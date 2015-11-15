package pl.metastack.metadocs.input.markdown

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
  val quotedString = P("\"" ~/ (stringChars | "\\\"").rep.! ~ "\"")
    .map(_.replaceAllLiterally("\\\"", "\""))
  val argument = P(identifier ~ "=" ~ quotedString).map(tree.Argument.Named.tupled)

  trait Node
  case class Verbatim(value: String) extends Node
  case class Text(value: String) extends Node
  case class Tag(tag: tree.Tag) extends Node

  val verbatim = P(
    (
      "`".rep(1) ~
      (!"`" ~ AnyChar).rep(1) ~
      "`".rep(1)
    ).!
  ).map(Verbatim)

  val tag = P(
    "[" ~
      identifier ~
      whitespaces ~
      argument.rep(sep=CharsWhile(_.isWhitespace) ~/).? ~
      whitespaces ~
    "]" ~
      !"(" ~ // Don't match Markdown links
      ("{" ~ Parser.nodes ~ "}").?
  ).map { case (ident, args, children) =>
    Tag(tree.Tag(ident, args.getOrElse(Seq.empty),
      children.getOrElse(Seq.empty)))
  }

  val text = P(
    CharsWhile(!Set('`', '[').contains(_)).!
  ).map(t => Text(t))

  val other = P(("`" | "[").!).map(Text)

  val parser = P((verbatim | tag | text | other).rep)

  def parse(input: String): Try[tree.Tag] = Try(tag.parse(input).get.value.tag)

  /** Ignores code blocks and verbatim expressions */
  def replace(input: String): (String, Seq[tree.Tag]) = {
    val parsed = parser.parse(input).get.value
    parsed.foldLeft(("", Seq.empty[tree.Tag])) { case ((accSt, accTags), cur) =>
      cur match {
        case Verbatim(v) => (accSt + v, accTags)
        case Text(t) => (accSt + t, accTags)
        case Tag(t) => (accSt + "%" + (accTags.length + 1), accTags ++ Seq(t))
      }
    }
  }
}
