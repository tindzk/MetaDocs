package pl.metastack.metadocs.document.tree

case class Chapter(id: Option[String],
                   title: String,
                   children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(Chapter(id, title, children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Chapter(id, title, children.flatMap(_.flatMap(f)): _*))
  def copy(id: Option[String] = id,
           title: String = title,
           children: Seq[Node] = children) =
    Chapter(id, title, children: _*)
  def updateChildren(children: Seq[Node]): Node = Chapter(id, title, children: _*)
}
