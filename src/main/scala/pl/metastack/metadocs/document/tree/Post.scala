package pl.metastack.metadocs.document.tree

import org.joda.time.DateTime

case class Post(id: Option[String],
                date: DateTime,
                title: String,
                description: Option[String],
                children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(Post(id, date, title, description, children.map(_.map(f)): _*))
  def copy(id: Option[String] = id, date: DateTime = date, title: String = title, description: Option[String] = description, children: Seq[Node] = children) =
    Post(id, date, title, description, children: _*)
}
