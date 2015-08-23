package pl.metastack.metadocs.document.tree

/**
 * @param caption If not provided, the name of the referenced chapter/section
 *                will be used instead.
 */
case class Jump(ref: String, caption: Option[String]) extends Node {
  def children: Seq[Node] = Seq.empty
  def block: Boolean = false
  def map(f: Node => Node): Node = f(this)
}
