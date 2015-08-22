package pl.metastack.metadocs.document.tree

case class Todo(children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = Todo(children.map(f): _*)
}
