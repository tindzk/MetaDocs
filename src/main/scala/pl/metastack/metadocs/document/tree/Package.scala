package pl.metastack.metadocs.document.tree

case class Package(value: String) extends Node {
  def children: Seq[Node] = Seq.empty
  def block: Boolean = false
  def map(f: Node => Node): Node = f(this)
  def flatMap(f: Node => Seq[Node]): Seq[Node] = f(this)
  def updateChildren(children: Seq[Node]): Node = copy()
}
