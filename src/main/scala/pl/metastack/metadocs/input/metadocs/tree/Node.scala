package pl.metastack.metadocs.input.metadocs.tree

import pl.metastack.metadocs.Errata

sealed trait Node {
  def text: String
}

case class Root(children: Seq[Node]) extends Node {
  override def text: String = children.map(_.text).mkString
}

case class Tag(name: String,
               arguments: Seq[Argument] = Seq.empty,
               children: Seq[Node] = Seq.empty) extends Node {
  def argumentValue(key: String): Option[String] =
    arguments.collectFirst {
      case Argument.Named(k, value) if k == key => value
    }

  def defaultArgument(errata: Errata): Option[String] = {
    if (arguments.count(_.isInstanceOf[Argument.Unnamed]) > 1)
      errata.error("Only one argument may be unnamed", this)

    arguments.collectFirst {
      case Argument.Unnamed(value) => value
    }
  }

  override def text: String = children.map {
    case Text(t) => t
    case Tag(_, _, c) => c.map(_.text).mkString
  }.mkString
}

case class Text(value: String) extends Node {
  override def text: String = value
}
