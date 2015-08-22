package pl.metastack.metadocs.document.tree

case class Subsection(id: Option[String],
                      title: String,
                      children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = Subsection(id, title, children.map(f): _*)
}
