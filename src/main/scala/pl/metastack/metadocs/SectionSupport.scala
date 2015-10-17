package pl.metastack.metadocs

/**
 * @author Matt Hicks <matt@outr.com>
 */
trait SectionSupport {
  private var sectionResults = Map.empty[String, Any]

  def sectionResult(name: String): Option[Any] = sectionResults.get(name)

  protected def section[R](name: String)(f: => R): R = {
    try {
      val r = f
      if (r != ()) {
        sectionResults += name -> r
      }
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
