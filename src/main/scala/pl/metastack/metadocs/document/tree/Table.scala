package pl.metastack.metadocs.document.tree

case class Column(children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(Column(children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Column(children.flatMap(_.flatMap(f)): _*))
  def updateChildren(children: Seq[Node]): Node = Column(children: _*)
}

case class Row(children: Column*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node =
    f(Row(children.map(_.map(f)).asInstanceOf[Seq[Column]]: _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Row(children.flatMap(_.flatMap(f)).asInstanceOf[Seq[Column]]: _*))
  def updateChildren(children: Seq[Node]): Node = Row(children.asInstanceOf[Seq[Column]]: _*)
}

case class Table(caption: Option[Seq[Node]],
                 headerRow: Row,
                 children: Row*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(
    f(Table(
      caption.map(_.map(f)),
      f(headerRow).asInstanceOf[Row],
      children.map(_.map(f)).asInstanceOf[Seq[Row]]: _*)))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Table(
      caption.map(_.flatMap(f)),
      f(headerRow).head.asInstanceOf[Row],
      children.flatMap(_.flatMap(f)).asInstanceOf[Seq[Row]]: _*))
  def updateChildren(children: Seq[Node]): Node =
    Table(caption, headerRow, children.asInstanceOf[Seq[Row]]: _*)
}
