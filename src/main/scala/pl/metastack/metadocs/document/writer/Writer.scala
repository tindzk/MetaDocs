package pl.metastack.metadocs.document.writer

import pl.metastack.metadocs.document.tree

trait Writer[N <: tree.Node, T] {
  def write: PartialFunction[N, T]
}
