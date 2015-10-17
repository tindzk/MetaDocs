package pl.metastack.metadocs.input.metadocs.tree

sealed trait Argument
object Argument {
  case class Named(key: String, value: String) extends Argument
  case class Unnamed(value: String) extends Argument
}
