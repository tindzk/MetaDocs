package pl.metastack.metadocs.document

import scala.collection.mutable

object Extractors {
  def footnotes(root: tree.Node): Seq[tree.Footnote] = {
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

  def references(root: tree.Root): References = {
    def iterate(node: tree.Node): Option[Reference] =
      node match {
        case tag @ tree.Chapter(id, caption, children @ _*) =>
          Some(Reference(caption, id, children.flatMap(iterate)))
        case tag @ tree.Section(id, caption, children @ _*) =>
          Some(Reference(caption, id, children.flatMap(iterate)))
        case tag @ tree.Subsection(id, caption, children @ _*) =>
          Some(Reference(caption, id, children.flatMap(iterate)))
        case _ => None
      }

    References(root.children.flatMap(iterate))
  }

  def chapters(root: tree.Node): Seq[tree.Chapter] =
    root.children.collect {
      case ch: tree.Chapter => ch
    }
}
