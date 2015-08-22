package pl.metastack.metadocs.document.tree

case class Italic(children: Node*) extends Node {
  def block: Boolean = false
  def map(f: Node => Node): Node = Italic(children.map(f): _*)
}
