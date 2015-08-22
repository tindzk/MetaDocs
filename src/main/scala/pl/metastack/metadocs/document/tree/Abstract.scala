package pl.metastack.metadocs.document.tree

case class Abstract(children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = Abstract(children.map(f): _*)
}
