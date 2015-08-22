package pl.metastack.metadocs.document.tree

case class Column(children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = Column(children.map(f): _*)
}

case class Row(children: Column*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node =
    Row(children.map(f).asInstanceOf[Seq[Column]]: _*)
}

case class Table(headerRow: Row, children: Row*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = Table(
    f(headerRow).asInstanceOf[Row],
    children.map(f).asInstanceOf[Seq[Row]]: _*)
}
