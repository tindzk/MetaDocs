package pl.metastack.metadocs.document.writer.html.document

import java.io.File

import pl.metastack.metadocs.FileUtils
import pl.metastack.metadocs.document.tree
import pl.metastack.metadocs.document.writer.html.{Components, Writer}
import pl.metastack.metadocs.document.{Meta, Extractors}

import pl.metastack.{metaweb => web}

object MultiPage {
  def writeIndex(root: tree.Root,
                 filePath: File,
                 cssPath: Option[String],
                 meta: Option[Meta],
                 tocDepth: Int,
                 referenceUrl: String => String) {
    val body = web.tree.immutable.PlaceholderSeqNode(Seq(
      Components.header(meta),
      Components.toc(root, tocDepth, referenceUrl),
      Components.`abstract`(meta)
    ))

    val result = Components.pageSkeleton(cssPath, meta, body)

    FileUtils.printToFile(new File(filePath, "index.html")) { fw =>
      fw.write(result.toHtml)
    }
  }

  def writeChapter(filePath: File,
                   writer: Writer,
                   chapters: Seq[tree.Chapter],
                   chapter: tree.Chapter,
                   cssPath: Option[String],
                   meta: Option[Meta]) {
    val footnotes = Extractors.footnotes(chapter)

    val index = chapters.indexOf(chapter)

    val previous =
      if (chapters.head == chapter) None
      else Some(chapters(index - 1))

    val next =
      if (chapters.last == chapter) None
      else Some(chapters(index + 1))

    val body = web.tree.immutable.PlaceholderSeqNode(Seq(
      Components.navigationHeader(meta, previous, next),
      writer.chapter.write(chapter),
      Components.footnotes(writer, footnotes)
    ))

    val result = Components.pageSkeleton(cssPath, meta, body)

    FileUtils.printToFile(new File(filePath, s"${chapter.id.get}.html")) { fw =>
      fw.write(result.toHtml)
    }
  }

  def write(root: tree.Root,
            outputPath: String,
            cssPath: Option[String],
            meta: Option[Meta],
            tocDepth: Int = 3) {
    val filePath = new File(outputPath)
    filePath.mkdirs()

    val references = Extractors.references(root)

    def referenceUrl(id: String): String = {
      val resolved = references.resolve(id)
      val chapter = references.chapterOf(resolved)
      val anchor =
        if (chapter == resolved) ""
        else s"#${resolved.id.get}"  // Reference to a section
      s"${chapter.id.get}.html$anchor"
    }

    val writer = new Writer(referenceUrl)

    writeIndex(root, filePath, cssPath, meta, tocDepth, referenceUrl)

    val chapters = Extractors.chapters(root)

    chapters.foreach { chapter =>
      writeChapter(filePath, writer, chapters, chapter, cssPath, meta)
    }
  }
}
