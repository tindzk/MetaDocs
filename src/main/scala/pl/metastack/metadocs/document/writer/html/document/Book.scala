package pl.metastack.metadocs.document.writer.html.document

import java.io.File

import pl.metastack.metadocs.document._
import pl.metastack.metadocs.document.tree
import pl.metastack.metadocs.document.writer.html.Writer

import pl.metastack.metaweb._
import pl.metastack.{metaweb => web}
import pl.metastack.metadocs.document.writer.html.Components

object Book {
  def bodyWrapper(body: web.tree.Node) = {
    val generatedWith = Components.generatedWith()
    htmlT"""
    <div id="wrapper">
      $body
      $generatedWith
    </div>
    """
  }

  def index(root: tree.Root,
            meta: Option[Meta],
            tocDepth: Int,
            referenceUrl: String => String) = {
    val body = web.tree.Container(Seq(
      Components.header(meta),
      Components.toc(root, tocDepth, referenceUrl),
      Components.`abstract`(meta)
    ))

    bodyWrapper(body)
  }

  def chapter(meta: Option[Meta],
              writer: Writer,
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

    val body = web.tree.Container(Seq(
      Components.navigationHeader(meta, previous, next),
      writer.chapter.write(chapter),
      Components.footnotes(writer, footnotes)
    ))

    bodyWrapper(body)
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

    val writer = new Writer(referenceUrl)

    val indexBody = index(root, meta, tocDepth, referenceUrl)
    val indexResult = skeleton(meta, None, indexBody)
    Document.writeHtml(filePath, "index", indexResult)

    val chapters = Extractors.chapters(root)

    chapters.foreach { chapter =>
      val body = this.chapter(meta, writer, chapters, chapter)
      val result = skeleton(meta, Some(chapter.title), body)
      Document.writeHtml(filePath, chapter.id.get, result)
    }
  }
}
