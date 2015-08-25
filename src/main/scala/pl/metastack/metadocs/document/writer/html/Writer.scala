package pl.metastack.metadocs.document.writer.html

import pl.metastack.metadocs.document.tree
import pl.metastack.metarx.Var

import pl.metastack.metaweb._
import pl.metastack.{metaweb => web}

class Writer(referenceUrl: String => String) {
  def children(n: tree.Node) = n.children.map(node.write)

  val headerColumn = WebWriter[tree.Column] { column =>
    html"<th>${children(column)}</th>"
  }

  val bodyColumn = WebWriter[tree.Column] { column =>
    html"<td>${children(column)}</td>"
  }

  val table = WebWriter[tree.Table] { table =>
    val headerRowColumns = table.headerRow.children.map(headerColumn.write)
    val headerRow = html"""<tr class="header">$headerRowColumns</tr>"""
    val header = html"""<thead>$headerRow</thead>"""

    val bodyRows = table.children.zipWithIndex.map { case (row, index) =>
      val columns = row.children.map(bodyColumn.write)

      if ((index % 2) == 0) html"""<tr class="even">$columns</tr>"""
      else html"""<tr class="odd">$columns</tr>"""
    }

    val body = html"""<tbody>$bodyRows</tbody>"""
    html"<table>$header$body</table>"
  }

  val jump = WebWriter[tree.Jump] { jump =>
    val href = referenceUrl(jump.ref)
    html"<a href=$href>${jump.caption.get}</a>"
  }

  val bold = WebWriter[tree.Bold] { bold =>
    html"<b>${children(bold)}</b>"
  }

  val italic = WebWriter[tree.Italic] { italic =>
    html"<i>${children(italic)}</i>"
  }

  val code = WebWriter[tree.Code] { code =>
    html"<code>${children(code)}</code>"
  }

  val subsection = WebWriter[tree.Subsection] { subsection =>
    web.tree.immutable.PlaceholderSeqNode(
      html"<h3 id=${subsection.id}>${subsection.title}</h3>" +:
        children(subsection))
  }

  val section = WebWriter[tree.Section] { section =>
    web.tree.immutable.PlaceholderSeqNode(
      html"<h2 id=${section.id}>${section.title}</h2>" +: children(section))
  }

  val chapter = WebWriter[tree.Chapter] { chapter =>
    web.tree.immutable.PlaceholderSeqNode(
      html"<h1 id=${chapter.id}>${chapter.title}</h1>" +: children(chapter))
  }

  val listItem = WebWriter[tree.ListItem] { listItem =>
    html"<li>${children(listItem)}</li>"
  }

  val list = WebWriter[tree.List] { list =>
    html"<ul>${children(list)}</ul>"
  }

  val sbt = WebWriter[tree.Sbt] { sbt =>
    if (sbt.hidden) web.tree.immutable.Null
    else html"""<pre class="sourceCode scala"><code>${sbt.code}</code></pre>"""
  }

  val scala = WebWriter[tree.Scala] { scala =>
    if (scala.hidden) web.tree.immutable.Null
    else {
      val code = html"""<pre class="sourceCode scala"><code>${scala.code}</code></pre>"""
      val result = scala.result.map { result =>
        Seq(
          html"<b>Output:</b>",
          html"""<pre class="sourceCode"><code>$result</code></pre>""")
      }

      web.tree.immutable.PlaceholderSeqNode(code +: result.getOrElse(Seq.empty))
    }
  }

  val shell = WebWriter[tree.Shell] { shell =>
    html"""<pre class="sourceCode shell"><code>${shell.code}</code></pre>"""
  }

  val todo = WebWriter[tree.Todo] { todo =>
    html"<div><b>Todo:</b> ${children(todo)}</div>"
  }

  val url = WebWriter[tree.Url] { url =>
    html"<a href=${url.href}>${children(url)}</a>"
  }

  val image = WebWriter[tree.Image] { image =>
    html"<img src=${image.href} />"
  }

  val paragraph = WebWriter[tree.Paragraph] { paragraph =>
    html"<p>${children(paragraph)}</p>"
  }

  val text = WebWriter[tree.Text] { text =>
    web.tree.immutable.Text(text.text)
  }

  val footnote = WebWriter[tree.Footnote] { fn =>
    val id = fn.id.get
    val target = s"#fn$id"
    val refId = s"fnref$id"
    val idString = id.toString
    html"""<a href=$target id=$refId class="footnote">[$idString]</a>"""
  }

  val node: WebWriter[tree.Node] =
    WebWriter.combine[tree.Node](
      table.asInstanceOf[WebWriter[tree.Node]],
      Seq(
        list, listItem, code, url, image, bold, italic, todo, shell, sbt, scala,
        chapter, section, subsection, paragraph, text, jump, footnote
      ).map(_.asInstanceOf[WebWriter[tree.Node]]): _*)

  val root = WebWriter[tree.Root] { root =>
    web.tree.immutable.PlaceholderSeqNode(root.children.map(node.write))
  }
}
