package pl.metastack.metadocs.document.writer

import java.io.File

import pl.metastack.metadocs.FileUtils
import pl.metastack.metadocs.document.{Meta, Heading, TableOfContents, tree}
import pl.metastack.metarx.Var

import pl.metastack.metaweb._
import pl.metastack.{metaweb => web}

import scala.reflect.ClassTag

trait WebWriter[N <: tree.Node] extends Writer[N, web.tree.Node]

object WebWriter {
  def apply[N <: tree.Node](f: N => web.tree.Node)
                           (implicit ct: ClassTag[N]): WebWriter[N] =
    new WebWriter[N] {
      def write: PartialFunction[N, web.tree.Node] = {
        case node: N => f(node)
      }
    }

  def combine[N <: tree.Node](writer: WebWriter[N],
              writers: WebWriter[N]*): WebWriter[N] =
    new WebWriter[N] {
      def write: PartialFunction[N, web.tree.Node] =
        writers.foldLeft(writer.write) { (acc, cur) =>
          acc.orElse(cur.write)
        }
    }
}

object HTML {
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

  val abstractNode = WebWriter[tree.Abstract] { abstractNode =>
    html"""<p><small><strong>Abstract: </strong><em>${children(abstractNode)}</em></small></p>"""
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
    web.tree.PlaceholderSeqNode(
      html"<h3 id=${subsection.id}>${subsection.title}</h3>" +:
        children(subsection))
  }

  val section = WebWriter[tree.Section] { section =>
    web.tree.PlaceholderSeqNode(
      html"<h2 id=${section.id}>${section.title}</h2>" +: children(section))
  }

  val chapter = WebWriter[tree.Chapter] { chapter =>
    web.tree.PlaceholderSeqNode(
      html"<h1 id=${chapter.id}>${chapter.title}</h1>" +: children(chapter))
  }

  val listItem = WebWriter[tree.ListItem] { listItem =>
    html"<li>${children(listItem)}</li>"
  }

  val list = WebWriter[tree.List] { list =>
    html"<ul>${children(list)}</ul>"
  }

  val sbt = WebWriter[tree.Sbt] { sbt =>
    if (sbt.hidden) web.tree.PlaceholderSeqNode(Seq())  // TODO Introduce web.tree.Null
    else html"""<pre class="sourceCode scala"><code>${sbt.code}</code></pre>"""
  }

  val scala = WebWriter[tree.Scala] { scala =>
    if (scala.hidden) web.tree.PlaceholderSeqNode(Seq())
    else {
      val code = html"""<pre class="sourceCode scala"><code>${scala.code}</code></pre>"""
      val result = scala.result.map { result =>
        Seq(
          html"<b>Output:</b>",
          html"""<pre class="sourceCode"><code>$result</code></pre>""")
      }

      web.tree.PlaceholderSeqNode(code +: result.getOrElse(Seq.empty))
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
    web.tree.Text(Var(text.text))
  }

  val node: WebWriter[tree.Node] =
    WebWriter.combine[tree.Node](
      table.asInstanceOf[WebWriter[tree.Node]],
      Seq(
        abstractNode, list, listItem, code, url, image, bold, italic, todo,
        shell, sbt, scala, chapter, section, subsection, paragraph, text
      ).map(_.asInstanceOf[WebWriter[tree.Node]]): _*)

  val root = WebWriter[tree.Root] { root =>
    web.tree.PlaceholderSeqNode(root.children.map(node.write))
  }
}

object HTMLDocument {
  def write(root: tree.Root,
            outputPath: String,
            cssPath: Option[String],
            meta: Option[Meta],
            toc: Option[TableOfContents]) {
    def iterateToc(node: Heading): web.tree.Node =
      node match {
        case Heading(caption, id, children) =>
          val childrenHtml = children.map { child =>
            html"<ul>${iterateToc(child)}</ul>"
          }

          val idAnchor = id.map(a => s"#$a")
          html"<li><a href=$idAnchor>$caption</a>$childrenHtml</li>"
      }

    val tocHtml = toc.map { t =>
      t.children.map { child =>
        html"""<ul>${iterateToc(child)}</ul>"""
      }
    }.getOrElse(Seq.empty)

    val documentHtml = HTML.root.write(root)

    val header = meta.map { m =>
      html"""
      <header>
        <h3 class="date">${m.date}</h3>
        <h1 class="title">${m.title}</h1>
        <h2 class="author">${m.author}</h2>
        <p class="affilation"><em>${m.affiliation}</em></p>
      </header>
      """
    }.getOrElse(web.tree.PlaceholderSeqNode(Seq.empty))

    val title = meta.map(_.title).getOrElse("")
    val language = meta.map(_.language).getOrElse("en-GB")

    val result = html"""
    <!DOCTYPE html>
    <html lang="$language">
      <head>
        <title>$title</title>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <meta name="generator" content="MetaDocs" />
        <link rel="stylesheet" type="text/css" href=$cssPath />
      </head>

      <body>
        <div id="wrapper">
          $header
          <nav id="toc">$tocHtml</nav>
          $documentHtml
          <p><small>Generated with <a href="http://github.com/MetaStack-pl/MetaDocs">MetaDocs</a>.</small></p>
        </div>
      </body>
    </html>
    """

    FileUtils.printToFile(new File(outputPath)) { fw =>
      fw.write(result.toHtml)
    }
  }
}
