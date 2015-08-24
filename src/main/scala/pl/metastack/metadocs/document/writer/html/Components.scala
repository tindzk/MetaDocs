package pl.metastack.metadocs.document.writer.html

import pl.metastack.metadocs.document.{Meta, tree, Heading, TableOfContents}

import pl.metastack.metaweb._
import pl.metastack.{metaweb => web}

object Components {
  def toc(tableOfContents: Option[TableOfContents]): web.tree.Node = {
    def iterate(node: Heading): web.tree.Node =
      node match {
        case Heading(caption, id, children) =>
          val childrenHtml = children.map { child =>
            html"<ul>${iterate(child)}</ul>"
          }

          val idAnchor = id.map(a => s"#$a")
          html"<li><a href=$idAnchor>$caption</a>$childrenHtml</li>"
      }

    tableOfContents.map { t =>
      val children =
        t.children.map { child =>
          html"""<ul>${iterate(child)}</ul>"""
        }

      html"""<nav id="toc">$children</nav>"""
    }.getOrElse(web.tree.Null)
  }

  def footnotes(footnotes: Seq[tree.Footnote]): web.tree.Node =
    if (footnotes.isEmpty) web.tree.Null
    else {
      val items = footnotes.map { fn =>
        val id = fn.id.get
        val fnId = s"fn$id"
        val target = s"#fnref$id"

        html"""
            <li id=$fnId>
              <p>
                ${Writers.children(fn)}
                <a href=$target class="reversefootnote">&#160;&#8617;</a>
              </p>
            </li>
          """
      }

      html"""
          <div class="footnotes">
            <hr />
            <ol>$items</ol>
          </div>
        """
    }

  def header(meta: Option[Meta]): web.tree.Node =
    meta.map { m =>
      html"""
        <header>
          <h3 class="date">${m.date}</h3>
          <h1 class="title">${m.title}</h1>
          <h2 class="author">${m.author}</h2>
          <p class="affilation"><em>${m.affiliation}</em></p>
        </header>
        """
    }.getOrElse(web.tree.Null)

  def pageSkeleton(cssPath: Option[String],
                   meta: Option[Meta],
                   body: web.tree.Node): web.tree.Node = {
    val title = meta.map(_.title).getOrElse("")
    val language = meta.map(_.language).getOrElse("en-GB")

    html"""
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
            $body
            <p><small>Generated with <a href="http://github.com/MetaStack-pl/MetaDocs">MetaDocs</a>.</small></p>
          </div>
        </body>
      </html>
      """
    }
}
