package pl.metastack.metadocs.document

import pl.metastack.metadocs.BlockResult

object CodeProcessor {
  def embedListings(notebook: Map[String, BlockResult])(root: tree.Root): tree.Root = {
    def iterate(node: tree.Node): tree.Node =
      node match {
        case scala: tree.Scala if scala.blockRef.nonEmpty =>
          val block = notebook.getOrElse(scala.blockRef.get,
            throw new Exception(s"Notebook doesn't define block '${scala.blockRef.get}'"))
          scala.copy(code = Some(block.code), result = block.result)

        case node: tree.Node => node
      }

    root.map(iterate).asInstanceOf[tree.Root]
  }
}
