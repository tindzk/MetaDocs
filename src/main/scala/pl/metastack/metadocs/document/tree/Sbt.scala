package pl.metastack.metadocs.document.tree

case class Sbt(id: String,
               project: Option[String],
               hidden: Boolean,
               code: String) extends Node {
  override def text: String = code
  def children: Seq[Node] = Seq.empty
  def block: Boolean = true
  def map(f: Node => Node): Node = f(this)
  def updateChildren(children: Seq[Node]): Node = Sbt(id, project, hidden, code)
}
