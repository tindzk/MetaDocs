package pl.metastack.metadocs.document

import scala.collection.mutable

object Extractors {
  def footnotes(root: tree.Root): Seq[tree.Footnote] = {
    val footnotes = mutable.ArrayBuffer.empty[tree.Footnote]

    // TODO Provide better way of iterating without change over the tree
    root.map {
      case fn: tree.Footnote =>
        footnotes += fn
        fn
      case n => n
    }

    footnotes
  }
}
