package pl.metastack.metadocs.document.tree

case class Subsection(id: Option[String],
                      title: String,
                      children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(Subsection(id, title, children.map(_.map(f)): _*))
  def copy(id: Option[String] = id, title: String = title, children: Seq[Node] = children) =
    Subsection(id, title, children: _*)
  def updateChildren(children: Seq[Node]): Node = Subsection(id, title, children: _*)
}
