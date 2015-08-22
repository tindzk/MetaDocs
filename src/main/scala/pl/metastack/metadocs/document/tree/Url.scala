package pl.metastack.metadocs.document.tree

case class Url(href: String, children: Node*) extends Node {
  def block: Boolean = false
  def map(f: Node => Node): Node = Url(href, children.map(f): _*)
}
