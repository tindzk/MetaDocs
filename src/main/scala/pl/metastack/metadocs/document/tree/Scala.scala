package pl.metastack.metadocs.document.tree

case class Scala(
  id: String = "",
  project: Option[String] = None,
  global: Boolean = false,
  printResult: Boolean = false,
  hidden: Boolean = false,
  `class`: Option[String] = None,
  section: Option[String] = None,
  code: String,
  result: Option[String] = None
) extends Node {
  override def text: String = code
  def children: Seq[Node] = Seq.empty
  def block: Boolean = true
  def map(f: Node => Node): Node = f(this)
  def updateChildren(children: Seq[Node]): Node = copy()
}
