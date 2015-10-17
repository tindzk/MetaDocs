package pl.metastack.metadocs.document.tree

sealed trait ScalaType
object ScalaType {
  case object Code extends ScalaType
  case object Imports extends ScalaType
  case object Object extends ScalaType
  case object Class extends ScalaType
  case object CaseClass extends ScalaType
  case object Trait extends ScalaType
  case object Section extends ScalaType
}

case class Scala(
  `type`: ScalaType = ScalaType.Code,
  value: String = "",
  code: Option[String] = None,
  file: Option[String] = None,
  result: Option[String] = None
) extends Node {
  override def text: String = code.get
  def children: Seq[Node] = Seq.empty
  def block: Boolean = true
  def map(f: Node => Node): Node = f(this)
  def flatMap(f: Node => Seq[Node]): Seq[Node] = f(this)
  def updateChildren(children: Seq[Node]): Node = copy()
}
