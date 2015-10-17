package pl.metastack.metadocs.output

import pl.metastack.metadocs.document.tree

trait Writer[N <: tree.Node, T] {
  def write: PartialFunction[N, T]
}
