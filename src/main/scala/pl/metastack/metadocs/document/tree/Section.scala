package pl.metastack.metadocs.document.tree

case class Section(id: Option[String],
                   title: String,
                   children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(Section(id, title, children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Section(id, title, children.flatMap(_.flatMap(f)): _*))
  def copy(id: Option[String] = id, title: String = title, children: Seq[Node] = children) =
    Section(id, title, children: _*)
  def updateChildren(children: Seq[Node]): Node = Section(id, title, children: _*)
}
