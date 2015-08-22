package pl.metastack.metadocs.document.tree

case class Chapter(id: Option[String],
                   title: String,
                   children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = Chapter(id, title, children.map(f): _*)
}
