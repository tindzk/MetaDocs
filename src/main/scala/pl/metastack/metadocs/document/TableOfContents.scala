package pl.metastack.metadocs.document

case class Heading(caption: String,
                   id: Option[String],
                   children: Seq[Heading]) {
  def format(level: Int): String = {
    val result = "".padTo(level, ' ') + "+ " + caption + "\n"
    result + children.map(_.format(level + 1)).mkString
  }
}

case class TableOfContents(children: Seq[Heading]) {
  override def toString = children.map(_.format(0)).mkString.init
}
