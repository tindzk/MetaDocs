package pl.metastack.metadocs.document.tree

case class Paragraph(children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = Paragraph(children.map(f): _*)
}
