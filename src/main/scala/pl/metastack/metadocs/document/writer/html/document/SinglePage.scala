package pl.metastack.metadocs.document.writer.html.document

import java.io.File

import pl.metastack.metadocs.FileUtils
import pl.metastack.metadocs.document.tree
import pl.metastack.metadocs.document.writer.html.{Components, Writer}
import pl.metastack.metadocs.document.{Meta, Extractors}

import pl.metastack.{metaweb => web}

object SinglePage {
  def write(root: tree.Root,
            outputPath: String,
            cssPath: Option[String],
            meta: Option[Meta],
            toc: Boolean,
            tocDepth: Int = 3) {
    def referenceUrl(id: String) = s"#$id"
    val writer = new Writer(referenceUrl)

    val footnotes = Extractors.footnotes(root)

    val body = web.tree.immutable.PlaceholderSeqNode(Seq(
      Components.header(meta),
      Components.toc(root, tocDepth, referenceUrl),
      Components.`abstract`(meta),
      writer.root.write(root),
      Components.footnotes(writer, footnotes)
    ))

    val result = Components.pageSkeleton(cssPath, meta, body)

    FileUtils.printToFile(new File(outputPath)) { fw =>
      fw.write(result.toHtml)
    }
  }
}
