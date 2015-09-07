package pl.metastack.metadocs.document.tree

case class Footnote(id: Option[Int], children: Node*) extends Node {
  def block: Boolean = false
  def map(f: Node => Node): Node = f(Footnote(id, children.map(_.map(f)): _*))
  def copy(id: Option[Int] = id, children: Seq[Node] = children) =
    Footnote(id, children: _*)
  def updateChildren(children: Seq[Node]): Node = Footnote(id, children: _*)
}
