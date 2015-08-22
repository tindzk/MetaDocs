package pl.metastack.metadocs.document.tree

case class Section(id: Option[String],
                   title: String,
                   children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = Section(id, title, children.map(f): _*)
}
