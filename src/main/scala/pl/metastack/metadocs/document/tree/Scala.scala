package pl.metastack.metadocs.document.tree

case class Scala(
  blockRef: Option[String] = None,
  code: Option[String] = None,
  result: Option[String] = None
) extends Node {
  override def text: String = code.get
  def children: Seq[Node] = Seq.empty
  def block: Boolean = true
  def map(f: Node => Node): Node = f(this)
  def flatMap(f: Node => Seq[Node]): Seq[Node] = f(this)
  def updateChildren(children: Seq[Node]): Node = copy()
}
