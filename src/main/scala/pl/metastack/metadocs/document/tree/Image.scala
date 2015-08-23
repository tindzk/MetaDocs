package pl.metastack.metadocs.document.tree

case class Image(href: String) extends Node {
  def children: Seq[Node] = Seq.empty
  def block: Boolean = true
  def map(f: Node => Node): Node = Image(href)
}
