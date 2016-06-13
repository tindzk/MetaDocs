package pl.metastack.metadocs.output

import pl.metastack.metaweb._
import pl.metastack.{metaweb => web}

import pl.metastack.metadocs.document.tree
import pl.metastack.metadocs.output.html.WebWriter

class HTML(referenceUrl: String => String) {
  def children(n: tree.Node): Seq[web.tree.Node] =
    n.children.flatMap(node.write)

  val headerColumn = WebWriter[tree.Column] { column =>
    Seq(html"<th>${children(column)}</th>")
  }

  val bodyColumn = WebWriter[tree.Column] { column =>
    Seq(html"<td>${children(column)}</td>")
  }

  val table = WebWriter[tree.Table] { table =>
    val headerRowColumns = table.headerRow.children.flatMap(headerColumn.write)
    val headerRow = html"""<tr class="header">$headerRowColumns</tr>"""
    val header = html"""<thead>$headerRow</thead>"""

    val bodyRows = table.children.zipWithIndex.map { case (row, index) =>
      val columns = row.children.flatMap(bodyColumn.write)

      if ((index % 2) == 0) html"""<tr class="even">$columns</tr>"""
      else html"""<tr class="odd">$columns</tr>"""
    }

    val caption = table.caption.fold[Seq[web.tree.Node]](Seq.empty) { case c =>
      val nodes = c.flatMap(node.write)
      Seq(html"<caption>$nodes</caption>")
    }

    val body = html"""<tbody>$bodyRows</tbody>"""
    Seq(html"<table>$header$body$caption</table>")
  }

  val jump = WebWriter[tree.Jump] { jump =>
    val href = referenceUrl(jump.ref)
    Seq(html"<a href=$href>${jump.caption.get}</a>")
  }

  val bold = WebWriter[tree.Bold] { bold =>
    Seq(html"<b>${children(bold)}</b>")
  }

  val italic = WebWriter[tree.Italic] { italic =>
    Seq(html"<i>${children(italic)}</i>")
  }

  val code = WebWriter[tree.Code] { code =>
    Seq(html"""<span class="code">${children(code)}</span>""")
  }

  val subsection = WebWriter[tree.Subsection] { subsection =>
    html"<h3 id=${subsection.id}>${subsection.title}</h3>" +:
      children(subsection)
  }

  val section = WebWriter[tree.Section] { section =>
    html"<h2 id=${section.id}>${section.title}</h2>" +: children(section)
  }

  val chapter = WebWriter[tree.Chapter] { chapter =>
    html"<h1 id=${chapter.id}>${chapter.title}</h1>" +: children(chapter)
  }

  val listItem = WebWriter[tree.ListItem] { listItem =>
    Seq(html"<li>${children(listItem)}</li>")
  }

  val list = WebWriter[tree.List] { list =>
    Seq(html"<ul>${children(list)}</ul>")
  }

  val `package` = WebWriter[tree.Package] { `package` =>
    Seq.empty
  }

  val scala = WebWriter[tree.Scala] { scala =>
    val code = html"""<pre class="sourceCode scala"><code data-lang="scala">${scala.code.get}</code></pre>"""
    val result = scala.result.map { result =>
      Seq(
        html"<b>Output:</b>",
        html"""<pre class="sourceCode"><code>$result</code></pre>""")
    }

    code +: result.getOrElse(Seq.empty)
  }

  val shell = WebWriter[tree.Shell] { shell =>
    Seq(html"""<pre class="sourceCode shell"><code>${shell.code}</code></pre>""")
  }

  val listing = WebWriter[tree.Listing] { listing =>
    Seq(html"""<pre class="sourceCode"><code>${listing.code}</code></pre>""")
  }

  val todo = WebWriter[tree.Todo] { todo =>
    Seq(html"<div><b>Todo:</b> ${children(todo)}</div>")
  }

  val url = WebWriter[tree.Url] { url =>
    Seq(html"<a href=${url.href}>${children(url)}</a>")
  }

  val image = WebWriter[tree.Image] { image =>
    Seq(html"<img src=${image.href} />")
  }

  val paragraph = WebWriter[tree.Paragraph] { paragraph =>
    Seq(html"<p>${children(paragraph)}</p>")
  }

  val quote = WebWriter[tree.Quote] { quote =>
    Seq(html"<blockquote>${children(quote)}</blockquote>")
  }

  val text = WebWriter[tree.Text] { text =>
    Seq(web.tree.Text(text.text))
  }

  val footnote = WebWriter[tree.Footnote] { fn =>
    val id = fn.id.get
    val target = s"#fn$id"
    val refId = s"fnref$id"
    val idString = id.toString
    Seq(html"""<a href=$target id=$refId class="footnote">[$idString]</a>""")
  }

  val node: WebWriter[tree.Node] =
    WebWriter.combine[tree.Node](
      table.asInstanceOf[WebWriter[tree.Node]],
      Seq(
        list, listItem, code, url, image, bold, italic, todo, shell, listing,
        scala, `package`, chapter, section, subsection, paragraph, quote, text,
        jump, footnote
      ).map(_.asInstanceOf[WebWriter[tree.Node]]): _*)

  val root = WebWriter[tree.Root] { root =>
    root.children.flatMap(node.write)
  }
}
