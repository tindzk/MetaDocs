package pl.metastack.metadocs.document.tree

case class Todo(children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(Todo(children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] = f(this)
  def updateChildren(children: Seq[Node]): Node = Todo(children: _*)
}
