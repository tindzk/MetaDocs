package pl.metastack.metadocs.output

import java.io.File

import pl.metastack.metadocs.FileUtils
import pl.metastack.metadocs.document.{Meta, Extractors, tree}

class Markdown(referenceUrl: String => String) {
  def children(n: tree.Node) = n.children.map(node.write)

  val headerColumn = StringWriter[tree.Column] { column =>
    children(column).mkString
  }

  val bodyColumn = StringWriter[tree.Column] { column =>
    children(column).mkString
  }

  val table = StringWriter[tree.Table] { table =>
    val headerRowColumns = table.headerRow.children.map(headerColumn.write)
    val header = "| " + headerRowColumns.mkString(" | ") + " |"

    val body = table.children.zipWithIndex.map { case (row, index) =>
      val columns = row.children.map(bodyColumn.write)
      "| " + columns.mkString(" | ") + " |"
    }.mkString("\n")

    header + "\n" + body
  }

  val jump = StringWriter[tree.Jump] { jump =>
    val href = referenceUrl(jump.ref)
    s"[${jump.caption.get}]($href)"
  }

  val bold = StringWriter[tree.Bold] { bold =>
    s"**${children(bold).mkString}**"
  }

  val italic = StringWriter[tree.Italic] { italic =>
    s"*${children(italic).mkString}*"
  }

  val code = StringWriter[tree.Code] { code =>
    s"`${children(code).mkString}`"
  }

  val subsection = StringWriter[tree.Subsection] { subsection =>
    s"### ${subsection.title}\n" + children(subsection).mkString + "\n"
  }

  val section = StringWriter[tree.Section] { section =>
    s"## ${section.title}\n" + children(section).mkString + "\n"
  }

  val chapter = StringWriter[tree.Chapter] { chapter =>
    s"# ${chapter.title}\n" + children(chapter).mkString + "\n"
  }

  val listItem = StringWriter[tree.ListItem] { listItem =>
    s"* ${children(listItem).mkString}"
  }

  val list = StringWriter[tree.List] { list =>
    children(list).mkString("\n")
  }

  val `package` = StringWriter[tree.Package] { `package` =>
    ""
  }

  val scala = StringWriter[tree.Scala] { scala =>
    val code = s"```scala\n${scala.code.get}\n```\n"
    val result = scala.result.map { result =>
      "\n**Output:**\n" +
      s"```\n$result\n```"
    }

    code + result.getOrElse("")
  }

  val shell = StringWriter[tree.Shell] { shell =>
    s"```bash\n${shell.code}\n```\n"
  }

  val listing = StringWriter[tree.Listing] { listing =>
    s"```\n${listing.code}\n```\n"
  }

  val todo = StringWriter[tree.Todo] { todo =>
    s"**Todo:** ${children(todo).mkString}"
  }

  val url = StringWriter[tree.Url] { url =>
    s"[${children(url).mkString}](${url.href})"
  }

  val image = StringWriter[tree.Image] { image =>
    // TODO Support caption
    s"![caption](${image.href})"
  }

  val paragraph = StringWriter[tree.Paragraph] { paragraph =>
    children(paragraph).mkString + "\n\n"
  }

  val quote = StringWriter[tree.Quote] { quote =>
    children(quote).mkString.lines.map("> " + _).mkString
  }

  val text = StringWriter[tree.Text] { text =>
    text.text
  }

  val footnote = StringWriter[tree.Footnote] { fn =>
    val id = fn.id.get
    s"[^$id]"
  }

  val node: StringWriter[tree.Node] =
    StringWriter.combine[tree.Node](
      table.asInstanceOf[StringWriter[tree.Node]],
      Seq(
        list, listItem, code, url, image, bold, italic, todo, shell, scala,
        listing, `package`, chapter, section, subsection, paragraph, quote,
        text, jump, footnote
      ).map(_.asInstanceOf[StringWriter[tree.Node]]): _*)

  val root = StringWriter[tree.Root] { root =>
    root.children.map(node.write).mkString
  }
}

object Markdown {
  def toc(root: tree.Root,
          maxDepth: Int,
          referenceUrl: String => String): String = {
    def render(caption: String,
               id: Option[String],
               children: Seq[String],
               depth: Int): String = {
      val childrenMd = children.map(child => "".padTo(depth + 1, ' ') + child)
      val childrenMdLeadingLine =
        if (childrenMd.isEmpty) Seq()
        else Seq("") ++ childrenMd
      val url = id.map(referenceUrl).getOrElse("")
      s"* [$caption]($url)${childrenMdLeadingLine.mkString("\n")}"
    }

    def iterate(node: tree.Node, depth: Int): Option[String] =
      node match {
        case _ if depth >= maxDepth => None
        case tag @ tree.Chapter(_, id, caption, children @ _*) =>
          Some(render(caption, id, children.flatMap(iterate(_, depth + 1)), depth))
        case tag @ tree.Section(id, caption, children @ _*) =>
          Some(render(caption, id, children.flatMap(iterate(_, depth + 1)), depth))
        case tag @ tree.Subsection(id, caption, children @ _*) =>
          Some(render(caption, id, children.flatMap(iterate(_, depth + 1)), depth))
        case _ => None
      }

    val toc = root.children.flatMap(iterate(_, 0))
      .mkString("\n")

    if (toc.isEmpty) ""
    else "## Table of contents\n" + toc
  }

  def footnotes(writer: Markdown, footnotes: Seq[tree.Footnote]): String =
    if (footnotes.isEmpty) ""
    else {
      val items = footnotes.map { fn =>
        val id = fn.id.get
        s"[^$id]: ${writer.children(fn).mkString}"
      }

      items.mkString("\n")
    }

  def header(meta: Option[Meta]): String = {
    meta.map { m =>
      val date = m.date.toString("MMMM YYYY", m.locale)

      s"$date\n\n" +
      s"# ${m.title}\n" +
      s"**Author:** ${m.author}\n\n" +
      s"**Affiliation:** ${m.affiliation}\n"
    }.getOrElse("")
  }

  def navigationHeader(meta: Option[Meta],
                       previous: Option[tree.Chapter],
                       next: Option[tree.Chapter],
                       sourcePath: Option[String]): String = {
    val previousMd = previous.map { ch =>
      val href = s"${ch.id.get}.md"
      s"""Previous chapter: [${ch.title}]($href)"""
    }.getOrElse("[Table of contents](index.md)")

    val nextMd = next.map { ch =>
      val href = s"${ch.id.get}.md"
      s"""Next chapter: [${ch.title}]($href)"""
    }.getOrElse("")

    val editMd = meta.flatMap(_.editSourceURL).flatMap { edit =>
      sourcePath.map { sp =>
        val href = edit + sp
        s"[Edit source]($href)"
      }
    }.getOrElse("")

    val items = Seq(previousMd, nextMd, editMd)
      .filter(_.nonEmpty)
      .foldLeft(Seq.empty[String]) { case (acc, cur) =>
        acc match {
          case Nil => Seq(cur)
          case a => a ++ Seq("  |  ", cur)
        }
      }

    items.mkString + "\n\n"
  }

  def `abstract`(meta: Option[Meta]): String =
    meta.map { m =>
      s"*Abstract:* ${m.`abstract`}"
    }.getOrElse("")

  def index(root: tree.Root,
            meta: Option[Meta],
            tocDepth: Int,
            referenceUrl: String => String) =
    header(meta) + "\n" +
    toc(root, tocDepth, referenceUrl) + "\n\n" +
    `abstract`(meta)

  def chapter(meta: Option[Meta],
              writer: Markdown,
              chapters: Seq[tree.Chapter],
              chapter: tree.Chapter): String = {
    val footnotes = Extractors.footnotes(chapter)

    val index = chapters.indexOf(chapter)

    val previous =
      if (chapters.head == chapter) None
      else Some(chapters(index - 1))

    val next =
      if (chapters.last == chapter) None
      else Some(chapters(index + 1))

    navigationHeader(meta, previous, next, meta.flatMap(_.editSourceURL)) +
    writer.chapter.write(chapter) +
    this.footnotes(writer, footnotes)
  }

  def writeMd(filePath: File, id: String, root: String): Unit =
    FileUtils.printToFile(new File(filePath, s"$id.md")) { fw =>
      fw.write(root)
    }

  def write(root: tree.Root,
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
      s"${chapter.id.get}.md$anchor"
    }

    val writer = new Markdown(referenceUrl)

    val body = index(root, meta, tocDepth, referenceUrl)
    writeMd(filePath, "index", body)

    val chapters = Extractors.chapters(root)

    chapters.foreach { chapter =>
      val body = this.chapter(meta, writer, chapters, chapter)
      writeMd(filePath, chapter.id.get, body)
    }
  }
}
