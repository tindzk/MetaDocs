package pl.metastack.metadocs.document.tree

case class Paragraph(children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(Paragraph(children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Paragraph(children.flatMap(_.flatMap(f)): _*))
  def updateChildren(children: Seq[Node]): Node = Paragraph(children: _*)
}
