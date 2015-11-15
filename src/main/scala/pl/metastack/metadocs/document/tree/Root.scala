package pl.metastack.metadocs.document.tree

case class Root(sourcePath: Option[String], children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node =
    f(Root(sourcePath, children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Root(sourcePath, children.flatMap(_.flatMap(f)): _*))
  def copy(sourcePath: Option[String] = sourcePath,
           children: Seq[Node] = children) =
    Root(sourcePath, children: _*)
  def updateChildren(children: Seq[Node]): Node = Root(sourcePath, children: _*)
}
