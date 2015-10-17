package pl.metastack.metadocs.document.tree

case class List(children: ListItem*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node =
    f(List(children.map(_.map(f)).asInstanceOf[Seq[ListItem]]: _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(List(children.flatMap(_.flatMap(f)).asInstanceOf[Seq[ListItem]]: _*))
  def updateChildren(children: Seq[Node]): Node = List(children.asInstanceOf[Seq[ListItem]]: _*)
}

case class ListItem(children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(ListItem(children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(ListItem(children.flatMap(_.flatMap(f)): _*))
  def updateChildren(children: Seq[Node]): Node = ListItem(children: _*)
}
