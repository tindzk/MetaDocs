package pl.metastack.metadocs

/**
 * @author Matt Hicks <matt@outr.com>
 */
trait SectionSupport {
  private var printedLines = List.empty[String]
  private var sectionResults = Map.empty[String, Seq[String]]

  protected def flush(): Seq[String] = {
    val lines = printedLines
    printedLines = List.empty
    lines
  }

  def println(x: Any): Unit =
    printedLines = printedLines ++ Seq(x.toString)

  def sectionResult(name: String): Option[Seq[String]] =
    sectionResults.get(name)

  protected def section[R](name: String)(f: => R): R = {
    try {
      val r = f
      val flushed = flush()
      if (flushed.nonEmpty) sectionResults += name -> flushed
      else if (r != ()) sectionResults += name -> Seq(r.toString)
      r
    } catch {
      case t: Throwable =>
        throw new RuntimeException(s"Section: $name failed in $getClass", t)
    }
  }

  protected def sectionNoExec[R](name: String)(f: => R): Unit = {
    // Nothing to do
  }
}
