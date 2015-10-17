package pl.metastack.metadocs.document.tree

case class Bold(children: Node*) extends Node {
  def block: Boolean = false
  def map(f: Node => Node): Node = f(Bold(children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Bold(children.flatMap(_.flatMap(f)): _*))
  def updateChildren(children: Seq[Node]): Node = Bold(children: _*)
}
