package pl.metastack.metadocs.document.tree

case class Root(children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(Root(children.map(_.map(f)): _*))
}
