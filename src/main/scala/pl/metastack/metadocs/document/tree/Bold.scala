package pl.metastack.metadocs.document.tree

case class Bold(children: Node*) extends Node {
  def block: Boolean = false
  def map(f: Node => Node): Node = Bold(children.map(f): _*)
}
