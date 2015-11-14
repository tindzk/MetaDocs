package pl.metastack.metadocs.document.tree

case class Listing(code: String) extends Node {
  override def text: String = code
  def children: Seq[Node] = Seq.empty
  def block: Boolean = true
  def map(f: Node => Node): Node = f(this)
  def flatMap(f: Node => Seq[Node]): Seq[Node] = f(this)
  def updateChildren(children: Seq[Node]): Node = Shell(code)
}
