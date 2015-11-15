package pl.metastack.metadocs.input.markdown

import scala.collection.JavaConversions._

import org.pegdown.{Extensions, PegDownProcessor}
import org.pegdown.ast._

import pl.metastack.metadocs.{TextHelpers, input, document}
import pl.metastack.metadocs.input.metadocs.InstructionSet

case class Conversion(generateId: String => Option[String])

object Pegdown {
  def level(node: document.tree.Node): Int =
    node match {
      case c: document.tree.Chapter => 1
      case s: document.tree.Section => 2
      case s: document.tree.Subsection => 3
      case _ => 0
    }

  def parse(input: String,
            conversion: Conversion = Conversion(_ => None)): document.tree.Root = {
    val processor = new PegDownProcessor(Extensions.ALL)
    val root = processor.parseMarkdown(input.toCharArray)
    val ast = dispatch(root, conversion)

    ast.map { node =>
      node.updateChildren(node.children
        .foldRight(Seq.empty[document.tree.Node])
      { case (cur, acc) =>
        if (acc.isEmpty) Seq(cur)
        else {
          val curLevel = level(cur)
          val inline = curLevel != 0
          if (inline) {
            val (left, right) = acc.span(n => level(n) == 0 || level(n) > curLevel)
            cur.updateChildren(cur.children ++ left) +: right
          } else cur +: acc
        }
      })
    }.asInstanceOf[document.tree.Root]
  }

  def replacePlaceholders(root: document.tree.Root,
                          placeholders: Seq[document.tree.Node]): document.tree.Root = {
    val number = "%\\d+"
    def split(s: String) = s.replaceAll(number, "_$0_").split("_")

    val result = root.flatMap {
      case t: document.tree.Text =>
        split(t.text).map { part =>
          if (part.matches(number)) placeholders(part.tail.toInt - 1)
          else document.tree.Text(part)
        }

      case n => Seq(n)
    }

    assert(result.size == 1)
    result.head.asInstanceOf[document.tree.Root]
  }

  def parseWithExtensions(ipt: String,
                          instructionSet: InstructionSet,
                          conversion: Conversion = Conversion(_ => None)
                         ): document.tree.Root = {
    val (parsedInput, placeholders) = input.markdown.BlockParser.replace(ipt)

    val inputConversion = new input.metadocs.Conversion(
      instructionSet, conversion.generateId)
    val parsedPlaceholders = placeholders.map(inputConversion.convertTag)

    replacePlaceholders(parse(parsedInput, conversion), parsedPlaceholders)
  }

  def visit(node: TextNode, conversion: Conversion): document.tree.Node =
    document.tree.Text(node.getText)

  def visit(node: WikiLinkNode, conversion: Conversion): document.tree.Node = ???

  def visit(node: VerbatimNode, conversion: Conversion): document.tree.Node = {
    if (node.getType == "scala")
      document.tree.Scala(code = Some(TextHelpers.reindent(node.getText)))
    else if (node.getType == "bash")
      document.tree.Shell(code = TextHelpers.reindent(node.getText))
    else if (node.getType == "")
      document.tree.Listing(code = TextHelpers.reindent(node.getText))
    else throw new RuntimeException(s"Unknown type in verbatim node: ${node.getType}")
  }

  def visit(node: TableNode, conversion: Conversion): document.tree.Node = {
    def processRow(node: Node): document.tree.Row = {
      val row = node.asInstanceOf[TableRowNode]
      val columns = row.getChildren.collect {
        case c: TableCellNode =>
          document.tree.Column(c.getChildren.map(dispatch(_, conversion)): _*)
      }
      document.tree.Row(columns: _*)
    }

    val header = node.getChildren.collectFirst {
      case header: TableHeaderNode => processRow(header.getChildren.head)
    }

    val body = node.getChildren.collect {
      case body: TableBodyNode => body.getChildren.map(processRow)
    }.flatten

    document.tree.Table(header.get, body: _*)
  }

  def visit(node: StrongEmphSuperNode, conversion: Conversion): document.tree.Node =
    if (node.isClosed) {
      if (node.isStrong) document.tree.Bold(node.getChildren.map(dispatch(_, conversion)): _*)
      else document.tree.Italic(node.getChildren.map(dispatch(_, conversion)): _*)
    } else document.tree.Text(node.getChars)

  def visit(node: AnchorLinkNode, conversion: Conversion): document.tree.Node = ???

  def visit(node: StrikeNode, conversion: Conversion): document.tree.Node = ???

  def visit(node: HtmlBlockNode, conversion: Conversion): document.tree.Node = ???

  def childrenText(node: Node): String =
    node.getChildren.collect {
      case t: TextNode => t.getText
    }.mkString

  def superChildrenText(node: Node): String =
    superChildren(node).collect {
      case t: TextNode => t.getText
    }.mkString

  def visit(node: HeaderNode, conversion: Conversion): document.tree.Node =
    node.getLevel match {
      case 1 => document.tree.Chapter(None,
        conversion.generateId(childrenText(node)), childrenText(node))
      case 2 => document.tree.Section(
        conversion.generateId(childrenText(node)), childrenText(node))
      case 3 => document.tree.Subsection(
        conversion.generateId(childrenText(node)), childrenText(node))
    }

  def visit(node: RefLinkNode, conversion: Conversion): document.tree.Node = {
    val url = superChildrenText(node)
    if (url.head == '#') document.tree.Jump(url.tail, None)
    else document.tree.Url(url)
  }

  def visit(node: ExpLinkNode, conversion: Conversion): document.tree.Node = {
    if (node.url.head == '#')
      document.tree.Jump(node.url.tail, Some(superChildrenText(node)))
    else document.tree.Url(node.url, superChildren(node).map(dispatch(_, conversion)): _*)
  }

  def visit(node: ExpImageNode, conversion: Conversion): document.tree.Node =
    document.tree.Image(node.url)

  def visit(node: DefinitionTermNode, conversion: Conversion): document.tree.Node = ???

  def visit(node: DefinitionNode, conversion: Conversion): document.tree.Node = ???

  def visit(node: DefinitionListNode, conversion: Conversion): document.tree.Node = ???

  def visit(node: CodeNode, conversion: Conversion): document.tree.Node =
    document.tree.Code(document.tree.Text(node.getText))

  def visit(node: BulletListNode, conversion: Conversion): document.tree.Node =
    document.tree.List(
      node.getChildren
        .map(dispatch(_, conversion).asInstanceOf[document.tree.ListItem]): _*
    )

  def visit(node: OrderedListNode, conversion: Conversion): document.tree.Node =
    document.tree.List(
      superChildren(node)
        .map(dispatch(_, conversion).asInstanceOf[document.tree.ListItem]): _*)

  def visit(node: BlockQuoteNode, conversion: Conversion): document.tree.Node =
    document.tree.Quote(node.getChildren.map(dispatch(_, conversion)): _*)

  def visit(node: AutoLinkNode, conversion: Conversion): document.tree.Node =
    document.tree.Url(node.getText, node.getChildren.map(dispatch(_, conversion)): _*)

  def visit(node: AbbreviationNode, conversion: Conversion): document.tree.Node = ???

  def visit(node: InlineHtmlNode, conversion: Conversion): document.tree.Node = ???

  def visit(node: ListItemNode, conversion: Conversion): document.tree.Node =
    document.tree.ListItem(rootSuperChildren(node).map(dispatch(_, conversion)): _*)

  def visit(node: MailLinkNode, conversion: Conversion): document.tree.Node = ???

  def visit(node: ParaNode, conversion: Conversion): document.tree.Node =
    document.tree.Paragraph(superChildren(node).map(dispatch(_, conversion)): _*)

  def visit(node: QuotedNode, conversion: Conversion): document.tree.Node =
    node.getType match {
      case QuotedNode.Type.DoubleAngle =>
        document.tree.Text("«" + childrenText(node) + "»")
      case QuotedNode.Type.Double =>
        document.tree.Text("\"" + childrenText(node) + "\"")
      case QuotedNode.Type.Single =>
        document.tree.Text("'" + childrenText(node) + "'")
    }

  def visit(node: ReferenceNode, conversion: Conversion): document.tree.Node = ???

  def visit(node: RefImageNode, conversion: Conversion): document.tree.Node = ???

  def visit(node: SimpleNode, conversion: Conversion): document.tree.Node = {
    val text = node.getType match {
      case SimpleNode.Type.Apostrophe => "’"
      case SimpleNode.Type.Ellipsis => "…"
      case SimpleNode.Type.Emdash => "—"
      case SimpleNode.Type.Endash => "–"
      case SimpleNode.Type.HRule => "<hr/>"  // TODO
      case SimpleNode.Type.Linebreak => "\n"
      case SimpleNode.Type.Nbsp => " "
    }

    document.tree.Text(text)
  }

  def visit(node: RootNode, conversion: Conversion): document.tree.Root =
    document.tree.Root(None, node.getChildren.map(dispatch(_, conversion)): _*)

  def dispatch(node: Node, conversion: Conversion): document.tree.Node =
    node match {
      case n: WikiLinkNode => visit(n, conversion)
      case n: VerbatimNode => visit(n, conversion)
      case n: TableNode => visit(n, conversion)
      case n: StrongEmphSuperNode => visit(n, conversion)
      case n: AnchorLinkNode => visit(n, conversion)
      case n: StrikeNode => visit(n, conversion)
      case n: HtmlBlockNode => visit(n, conversion)
      case n: HeaderNode => visit(n, conversion)
      case n: ExpLinkNode => visit(n, conversion)
      case n: ExpImageNode => visit(n, conversion)
      case n: DefinitionTermNode => visit(n, conversion)
      case n: DefinitionNode => visit(n, conversion)
      case n: DefinitionListNode => visit(n, conversion)
      case n: CodeNode => visit(n, conversion)
      case n: BulletListNode => visit(n, conversion)
      case n: BlockQuoteNode => visit(n, conversion)
      case n: AutoLinkNode => visit(n, conversion)
      case n: AbbreviationNode => visit(n, conversion)
      case n: InlineHtmlNode => visit(n, conversion)
      case n: ListItemNode => visit(n, conversion)
      case n: MailLinkNode => visit(n, conversion)
      case n: OrderedListNode => visit(n, conversion)
      case n: ParaNode => visit(n, conversion)
      case n: QuotedNode => visit(n, conversion)
      case n: ReferenceNode => visit(n, conversion)
      case n: RefImageNode => visit(n, conversion)
      case n: RefLinkNode => visit(n, conversion)
      case n: RootNode => visit(n, conversion)
      case n: TextNode => visit(n, conversion)
      case n: SimpleNode => visit(n, conversion)
      case n => throw new RuntimeException(s"Unknown node '$n'")
    }

  def superChildren(node: Node) =
    node.getChildren.head.asInstanceOf[SuperNode].getChildren

  def rootSuperChildren(node: Node) =
    superChildren(node.getChildren.head.asInstanceOf[RootNode])
}
