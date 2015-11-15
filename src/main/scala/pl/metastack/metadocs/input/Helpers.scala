package pl.metastack.metadocs.input

object Helpers {
  def replaceConstants(raw: String, constants: Map[String, String]): String =
    constants.foldLeft(raw) { case (acc, (k, v)) =>
      acc.replaceAllLiterally(s"%$k%", v)
    }
}
