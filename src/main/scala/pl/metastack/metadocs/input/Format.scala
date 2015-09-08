package pl.metastack.metadocs.input

sealed trait Format

object Format {
  case object Markdown extends Format
  case object MetaDocs extends Format
}
