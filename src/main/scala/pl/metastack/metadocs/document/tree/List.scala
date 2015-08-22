package pl.metastack.metadocs.document.tree

case class List(children: ListItem*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node =
    List(children.map(f).asInstanceOf[Seq[ListItem]]: _*)
}

case class ListItem(children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = ListItem(children.map(f): _*)
}
