package pl.metastack.metadocs.document.tree

/** Block quote */
case class Quote(children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(Quote(children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Quote(children.flatMap(_.flatMap(f)): _*))
  def updateChildren(children: Seq[Node]): Node = Quote(children: _*)
}
