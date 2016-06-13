package pl.metastack.metadocs.output.html.document

import java.io.File

import pl.metastack.metadocs.output.HTML
import pl.metastack.metaweb._
import pl.metastack.{metaweb => web}

import pl.metastack.metadocs.document._
import pl.metastack.metadocs.document.tree
import pl.metastack.metadocs.output.html.Components

object Book {
  def index(root: tree.Root,
            meta: Option[Meta],
            tocDepth: Int,
            referenceUrl: String => String): web.tree.Node = {
    val body = Seq(
      Components.header(meta),
      Components.toc(root, tocDepth, referenceUrl),
      Components.`abstract`(meta)
    ).flatten

    Components.bodyWrapper(body)
  }

  def chapter(meta: Option[Meta],
              writer: HTML,
              chapters: Seq[tree.Chapter],
              chapter: tree.Chapter): web.tree.Node = {
    val footnotes = Extractors.footnotes(chapter)

    val index = chapters.indexOf(chapter)

    val previous =
      if (chapters.head == chapter) None
      else Some(chapters(index - 1))

    val next =
      if (chapters.last == chapter) None
      else Some(chapters(index + 1))

    val body = Seq(
      Components.navigationHeader(meta, previous, next, chapter.sourcePath),
      writer.chapter.write(chapter),
      Components.footnotes(writer, footnotes).toSeq
    ).flatten

    Components.bodyWrapper(body)
  }

  def write(root: tree.Root,
            skeleton: Components.Skeleton,
            outputPath: String,
            meta: Option[Meta],
            tocDepth: Int = 3) {
    val filePath = new File(outputPath)
    filePath.mkdirs()

    val references = Extractors.references(root)

    def referenceUrl(id: String): String = {
      val resolved = references.resolve(id)
      val chapter = references.topLevelReferenceOf(resolved)
      val anchor =
        if (chapter == resolved) ""
        else s"#${resolved.id.get}"  // Reference to a section
      s"${chapter.id.get}.html$anchor"
    }

    val writer = new HTML(referenceUrl)

    val indexBody = index(root, meta, tocDepth, referenceUrl)
    val indexResult = skeleton(meta, None, Seq(indexBody))
    Document.writeHtml(filePath, "index", indexResult)

    val chapters = Extractors.chapters(root)

    chapters.foreach { chapter =>
      val body = this.chapter(meta, writer, chapters, chapter)
      val result = skeleton(meta, Some(chapter.title), Seq(body))
      Document.writeHtml(filePath, chapter.id.get, result)
    }
  }
}
