package pl.metastack.metadocs.document.tree

case class Text(override val text: String) extends Node {
  def children: Seq[Node] = Seq.empty
  def block: Boolean = false
  def map(f: Node => Node): Node = this
}
