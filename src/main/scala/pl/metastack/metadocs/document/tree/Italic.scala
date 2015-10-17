package pl.metastack.metadocs.document.tree

case class Italic(children: Node*) extends Node {
  def block: Boolean = false
  def map(f: Node => Node): Node = f(Italic(children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Italic(children.flatMap(_.flatMap(f)): _*))
  def updateChildren(children: Seq[Node]): Node = Italic(children: _*)
}
