package pl.metastack.metadocs.document.tree

case class Column(children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(Column(children.map(_.map(f)): _*))
}

case class Row(children: Column*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node =
    f(Row(children.map(_.map(f)).asInstanceOf[Seq[Column]]: _*))
}

case class Table(headerRow: Row, children: Row*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(
    Table(
        f(headerRow).asInstanceOf[Row],
        children.map(_.map(f)).asInstanceOf[Seq[Row]]: _*))
}
