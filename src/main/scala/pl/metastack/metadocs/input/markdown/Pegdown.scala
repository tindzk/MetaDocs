package pl.metastack.metadocs.input.markdown

import scala.collection.JavaConversions._

import org.pegdown.{Extensions, PegDownProcessor}
import org.pegdown.ast._

import pl.metastack.metadocs.document
import pl.metastack.metadocs.input.TextHelpers

object Pegdown {
  def level(node: document.tree.Node): Int =
    node match {
      case c: document.tree.Chapter => 1
      case s: document.tree.Section => 2
      case s: document.tree.Subsection => 3
      case _ => 0
    }

  def parse(input: String): document.tree.Node = {
    val processor = new PegDownProcessor(Extensions.ALL)
    val root = processor.parseMarkdown(input.toCharArray)
    val ast = dispatch(root)

    ast.map { node =>
      node.updateChildren(node.children.foldRight(Seq.empty[document.tree.Node]) { case (cur, acc) =>
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
    }
  }

  def visit(node: TextNode): document.tree.Node =
    document.tree.Text(node.getText)

  def visit(node: WikiLinkNode): document.tree.Node = ???

  def visit(node: VerbatimNode): document.tree.Node = {
    if (node.getType == "scala")
      document.tree.Scala(code = TextHelpers.reindent(node.getText))
    else throw new RuntimeException(s"Unknown type in verbatim node: ${node.getType}")
  }

  def visit(node: TableRowNode): document.tree.Node = ???

  def visit(node: TableNode): document.tree.Node = ???

  def visit(node: TableHeaderNode): document.tree.Node = ???

  def visit(node: TableColumnNode): document.tree.Node = ???

  def visit(node: TableCellNode): document.tree.Node = ???

  def visit(node: TableCaptionNode): document.tree.Node = ???

  def visit(node: TableBodyNode): document.tree.Node = ???

  def visit(node: StrongEmphSuperNode): document.tree.Node =
    if (node.isStrong) document.tree.Bold(node.getChildren.map(dispatch): _*)
    else ???

  def visit(node: AnchorLinkNode): document.tree.Node =
    document.tree.Jump(node.getName, Some(getText(node)))  // TODO Test case

  def visit(node: StrikeNode): document.tree.Node = ???

  def visit(node: HtmlBlockNode): document.tree.Node = ???

  def getText(node: Node): String =
    node.getChildren.head.asInstanceOf[TextNode].getText

  def visit(node: HeaderNode): document.tree.Node =
    node.getLevel match {
      case 1 => document.tree.Chapter(None, getText(node))
      case 2 => document.tree.Section(None, getText(node))
      case 3 => document.tree.Subsection(None, getText(node))
    }

  def visit(node: ExpLinkNode): document.tree.Node =
    document.tree.Url(node.url, superChildren(node).map(dispatch): _*)

  def visit(node: ExpImageNode): document.tree.Node =
    document.tree.Image(node.url)

  def visit(node: DefinitionTermNode): document.tree.Node = ???

  def visit(node: DefinitionNode): document.tree.Node = ???

  def visit(node: DefinitionListNode): document.tree.Node = ???

  def visit(node: CodeNode): document.tree.Node =
    document.tree.Code(document.tree.Text(node.getText))

  def visit(node: BulletListNode): document.tree.Node =
    document.tree.List(
      node.getChildren
        .map(dispatch(_).asInstanceOf[document.tree.ListItem]): _*
    )

  def visit(node: OrderedListNode): document.tree.Node =
    document.tree.List(
      superChildren(node)
        .map(dispatch(_).asInstanceOf[document.tree.ListItem]): _*
    )

  def visit(node: BlockQuoteNode): document.tree.Node = ???

  def visit(node: AutoLinkNode): document.tree.Node = ???

  def visit(node: AbbreviationNode): document.tree.Node = ???

  def visit(node: InlineHtmlNode): document.tree.Node = ???

  def visit(node: ListItemNode): document.tree.Node =
    document.tree.ListItem(rootSuperChildren(node).map(dispatch): _*)

  def visit(node: MailLinkNode): document.tree.Node = ???

  def visit(node: ParaNode): document.tree.Node =
    document.tree.Paragraph(superChildren(node).map(dispatch): _*)

  def visit(node: QuotedNode): document.tree.Node = ???

  def visit(node: ReferenceNode): document.tree.Node = ???

  def visit(node: RefImageNode): document.tree.Node = ???

  def visit(node: RefLinkNode): document.tree.Node = ???

  def visit(node: RootNode): document.tree.Root =
    document.tree.Root(node.getChildren.map(dispatch): _*)

  def dispatch(node: Node): document.tree.Node =
    node match {
      case n: WikiLinkNode => visit(n)
      case n: VerbatimNode => visit(n)
      case n: TableRowNode => visit(n)
      case n: TableNode => visit(n)
      case n: TableHeaderNode => visit(n)
      case n: TableColumnNode => visit(n)
      case n: TableCellNode => visit(n)
      case n: TableCaptionNode => visit(n)
      case n: TableBodyNode => visit(n)
      case n: StrongEmphSuperNode => visit(n)
      case n: AnchorLinkNode => visit(n)
      case n: StrikeNode => visit(n)
      case n: HtmlBlockNode => visit(n)
      case n: HeaderNode => visit(n)
      case n: ExpLinkNode => visit(n)
      case n: ExpImageNode => visit(n)
      case n: DefinitionTermNode => visit(n)
      case n: DefinitionNode => visit(n)
      case n: DefinitionListNode => visit(n)
      case n: CodeNode => visit(n)
      case n: BulletListNode => visit(n)
      case n: BlockQuoteNode => visit(n)
      case n: AutoLinkNode => visit(n)
      case n: AbbreviationNode => visit(n)
      case n: InlineHtmlNode => visit(n)
      case n: ListItemNode => visit(n)
      case n: MailLinkNode => visit(n)
      case n: OrderedListNode => visit(n)
      case n: ParaNode => visit(n)
      case n: QuotedNode => visit(n)
      case n: ReferenceNode => visit(n)
      case n: RefImageNode => visit(n)
      case n: RefLinkNode => visit(n)
      case n: RootNode => visit(n)
      case n: TextNode => visit(n)
      case n => throw new RuntimeException(s"Unknown node '$n'")
    }

  def superChildren(node: Node) =
    node.getChildren.head.asInstanceOf[SuperNode].getChildren

  def rootSuperChildren(node: Node) =
    superChildren(node.getChildren.head.asInstanceOf[RootNode])
}
