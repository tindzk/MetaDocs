package pl.metastack.metadocs.document

case class Reference(caption: String,
                     id: Option[String],
                     children: Seq[Reference]) {
  def format(level: Int): String = {
    val result = "".padTo(level, ' ') + "+ " + caption + "\n"
    result + children.map(_.format(level + 1)).mkString
  }
}

case class References(children: Seq[Reference]) {
  override def toString = children.map(_.format(0)).mkString.init

  def resolve(id: String): Reference = {
    def f(node: Reference): Option[Reference] =
      node match {
        case heading @ Reference(_, Some(`id`), _) => Some(heading)
        case Reference(_, _, children) =>
          children.collectFirst {
            case c if f(c).isDefined => f(c).get  // TODO Call f() only once
          }
      }

    children.collectFirst {
      case c if f(c).isDefined => f(c).get
    }.get
  }

  def chapterOf(needle: Reference): Reference = {
    def f(chapter: Reference, node: Reference): Boolean =
      node match {
        case `needle` => true
        case Reference(_, _, ch) => ch.exists(f(chapter, _))
      }

    children.find(chapter => f(chapter, chapter)).get
  }
}
