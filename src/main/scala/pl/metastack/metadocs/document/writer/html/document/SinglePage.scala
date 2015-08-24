package pl.metastack.metadocs.document.writer.html.document

import java.io.File

import pl.metastack.metadocs.FileUtils
import pl.metastack.metadocs.document.tree
import pl.metastack.metadocs.document.writer.html.{Components, Writers}
import pl.metastack.metadocs.document.{Meta, TableOfContents, Extractors}

import pl.metastack.{metaweb => web}

object SinglePage {
  def write(root: tree.Root,
            outputPath: String,
            cssPath: Option[String],
            meta: Option[Meta],
            toc: Option[TableOfContents]) {
    val footnotes = Extractors.footnotes(root)

    val body = web.tree.PlaceholderSeqNode(Seq(
      Components.header(meta),
      Components.toc(toc),
      Writers.root.write(root),
      Components.footnotes(footnotes)
    ))

    val result = Components.pageSkeleton(cssPath, meta, body)

    FileUtils.printToFile(new File(outputPath)) { fw =>
      fw.write(result.toHtml)
    }
  }
}
