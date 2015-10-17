package pl.metastack.metadocs.document.tree

case class Root(children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(Root(children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Root(children.flatMap(_.flatMap(f)): _*))
  def updateChildren(children: Seq[Node]): Node = Root(children: _*)
}
