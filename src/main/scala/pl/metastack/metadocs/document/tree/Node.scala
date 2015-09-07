package pl.metastack.metadocs.document.tree

trait Node {
  /** If this is a block node, no surrounding paragraph is necessary */
  def block: Boolean

  def text: String = children.map(_.text).mkString

  def children: Seq[Node]
  def map(f: Node => Node): Node

  def updateChildren(children: Seq[Node]): Node
}
