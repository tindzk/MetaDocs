package pl.metastack.metadocs.document.tree

case class Chapter(sourcePath: Option[String],
                   id: Option[String],
                   title: String,
                   children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(Chapter(sourcePath, id, title, children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Chapter(sourcePath, id, title, children.flatMap(_.flatMap(f)): _*))
  def copy(sourcePath: Option[String] = sourcePath,
           id: Option[String] = id,
           title: String = title,
           children: Seq[Node] = children) =
    Chapter(sourcePath, id, title, children: _*)
  def updateChildren(children: Seq[Node]): Node = Chapter(sourcePath, id, title, children: _*)
}
