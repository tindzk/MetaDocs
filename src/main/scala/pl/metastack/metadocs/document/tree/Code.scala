package pl.metastack.metadocs.document.tree

/** Inline code */
case class Code(children: Node*) extends Node {
  def block: Boolean = false
  def map(f: Node => Node): Node = f(Code(children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Code(children.flatMap(_.flatMap(f)): _*))
  def updateChildren(children: Seq[Node]): Node = Code(children: _*)
}
