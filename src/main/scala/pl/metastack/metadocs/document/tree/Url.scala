package pl.metastack.metadocs.document.tree

case class Url(href: String, children: Node*) extends Node {
  def block: Boolean = false
  def map(f: Node => Node): Node = f(Url(href, children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] = f(this)
  def updateChildren(children: Seq[Node]): Node = Url(href, children: _*)
}
