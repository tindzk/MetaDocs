package pl.metastack.metadocs.output.html.document

import java.io.File

import pl.metastack.metadocs.FileUtils
import pl.metastack.metadocs.document.tree
import pl.metastack.metadocs.document.{Meta, Extractors}
import pl.metastack.metadocs.output.HTML
import pl.metastack.metadocs.output.html.Components

import pl.metastack.metaweb._

object SinglePage {
  def write(root: tree.Root,
            skeleton: Components.Skeleton,
            outputPath: String,
            meta: Option[Meta],
            toc: Boolean,
            tocDepth: Int = 3): Unit = {
    def referenceUrl(id: String) = s"#$id"
    val writer = new HTML(referenceUrl)

    val footnotes = Extractors.footnotes(root)

    val body = Seq(
      Components.header(meta),
      Components.toc(root, tocDepth, referenceUrl),
      Components.`abstract`(meta)
    ).flatten ++ writer.root.write(root) ++
      Components.footnotes(writer, footnotes).toSeq

    val result = skeleton(meta, None, Seq(Components.bodyWrapper(body)))

    FileUtils.printToFile(new File(outputPath)) { fw =>
      fw.write(result.toHtml)
    }
  }
}
