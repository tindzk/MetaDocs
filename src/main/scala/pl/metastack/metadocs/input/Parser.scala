package pl.metastack.metadocs.input

import fastparse.all._

import scala.util.Try

object Parser {
  val controlChars = Set(',', '=', '[', ']', '{', '}', '"', '(')

  val identifier = P(CharsWhile(
    c => !c.isWhitespace && !controlChars.contains(c)
  ).!)

  val whitespaces = P(CharsWhile(_.isWhitespace).?)

  val stringChars = P(CharsWhile(!Set('\\', '\"').contains(_)))
  val quotedString = P("\"" ~! (stringChars | "\\\"").rep.! ~ "\"")
    .map(_.replaceAllLiterally("\\\"", "\""))
  val argumentValue = P(quotedString | CharsWhile(!controlChars.contains(_)).!)
  val namedArgument = P(identifier ~ "=" ~ argumentValue).map(tree.Argument.Named.tupled)
  val unnamedArgument = P(argumentValue).map(tree.Argument.Unnamed)
  val argument: Parser[tree.Argument] = P(namedArgument | unnamedArgument)

  val text = P(
    (
      "\\}" |
      CharsWhile(_.isWhitespace) |
      (!(tag | "}") ~ AnyChar)
    ).rep(1).!
  ).map(s => tree.Text(s.replaceAllLiterally("\\}", "}")))

  val rawText = P(("\\*}" | (!"*}" ~ AnyChar)).rep(1).!)
    .map(s => tree.Text(s.replaceAllLiterally("\\*}", "*}")))

  val tag = P(
    whitespaces ~
    identifier ~
    ("[" ~ argument.rep(sep="," ~!) ~ "]").? ~
    whitespaces ~ (
      "{*" ~ rawText.rep ~ "*}" |
      "{" ~ nodes ~ "}"
    )
  ).map { case (ident, args, children) =>
    tree.Tag(ident, args.getOrElse(Seq.empty), children)
  }

  val node: Parser[tree.Node] = P(tag | text)
  val nodes = P(node.rep)

  val root = P(nodes ~ End).map(tree.Root)

  def parse(input: String): Try[tree.Root] = Try(root.parse(input).get.value)
}
