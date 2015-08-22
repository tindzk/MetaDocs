package pl.metastack.metadocs.document.tree

/** Inline code */
case class Code(children: Node*) extends Node {
  def block: Boolean = false
  def map(f: Node => Node): Node = Code(children.map(f): _*)
}
