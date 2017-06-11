package pl.metastack.metadocs.output.html.document

import pl.metastack.metadocs.document.tree
import pl.metastack.metadocs.document.{Extractors, Meta}
import pl.metastack.metadocs.output.HTML
import pl.metastack.metadocs.output.html.Components
import pl.metastack.metaweb

object SinglePage {
  def compose(root: tree.Root,
              skeleton: Components.Skeleton,
              meta: Option[Meta],
              toc: Boolean,
              tocDepth: Int = 3): metaweb.tree.Node = {
    def referenceUrl(id: String) = s"#$id"
    val writer = new HTML(referenceUrl)

    val footnotes = Extractors.footnotes(root)

    val body = Seq(
      Components.header(meta),
      Components.toc(root, tocDepth, referenceUrl),
      Components.`abstract`(meta)
    ).flatten ++ writer.root.write(root) ++
      Components.footnotes(writer, footnotes).toSeq

    skeleton(meta, None, Seq(Components.bodyWrapper(body)))
  }
}
