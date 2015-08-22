package pl.metastack.metadocs.document.tree

case class Scala(
  id: String,
  project: Option[String],
  global: Boolean,
  printResult: Boolean,
  hidden: Boolean,
  code: String,
  result: Option[String] = None
) extends Node {
  override def text: String = code
  def children: Seq[Node] = Seq.empty
  def block: Boolean = true
  def map(f: Node => Node): Node = this
}
