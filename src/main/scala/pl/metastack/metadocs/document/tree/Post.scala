package pl.metastack.metadocs.document.tree

import org.joda.time.DateTime

case class Post(sourcePath: Option[String],
                id: Option[String],
                date: DateTime,
                title: String,
                description: Option[String],
                children: Node*) extends Node {
  def block: Boolean = true
  def map(f: Node => Node): Node = f(Post(sourcePath, id, date, title, description, children.map(_.map(f)): _*))
  def flatMap(f: Node => Seq[Node]): Seq[Node] =
    f(Post(sourcePath, id, date, title, description, children.flatMap(_.flatMap(f)): _*))
  def copy(sourcePath: Option[String] = sourcePath, id: Option[String] = id, date: DateTime = date, title: String = title, description: Option[String] = description, children: Seq[Node] = children) =
    Post(sourcePath, id, date, title, description, children: _*)
  def updateChildren(children: Seq[Node]): Node = Post(sourcePath, id, date, title, description, children: _*)
}
