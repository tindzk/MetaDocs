package pl.metastack.metadocs.document.writer.html

import pl.metastack.metadocs.document.{tree, Meta}

import pl.metastack.metaweb._
import pl.metastack.{metaweb => web}

object Components {
  def toc(root: tree.Root,
          maxDepth: Int,
          referenceUrl: String => String): web.tree.Node = {
    def render(caption: String,
               id: Option[String],
               children: Seq[web.tree.Node]): web.tree.Node = {
      val childrenHtml = children.map(child => html"<ul>$child</ul>")

      val url = id.map(referenceUrl)
      html"<li><a href=$url>$caption</a>$childrenHtml</li>"
    }

    def iterate(node: tree.Node, depth: Int): Option[web.tree.Node] =
      node match {
        case _ if depth >= maxDepth => None
        case tag @ tree.Chapter(id, caption, children @ _*) =>
          Some(render(caption, id, children.flatMap(iterate(_, depth + 1))))
        case tag @ tree.Section(id, caption, children @ _*) =>
          Some(render(caption, id, children.flatMap(iterate(_, depth + 1))))
        case tag @ tree.Subsection(id, caption, children @ _*) =>
          Some(render(caption, id, children.flatMap(iterate(_, depth + 1))))
        case _ => None
      }

    val toc = root.children.flatMap(iterate(_, 0))

    if (toc.isEmpty) web.tree.Null
    else html"""<nav id="toc"><ul>$toc</ul></nav>"""
  }

  def footnotes(writer: Writer, footnotes: Seq[tree.Footnote]): web.tree.Node =
    if (footnotes.isEmpty) web.tree.Null
    else {
      val items = footnotes.map { fn =>
        val id = fn.id.get
        val fnId = s"fn$id"
        val target = s"#fnref$id"

        html"""
            <li id=$fnId>
              <p>
                ${writer.children(fn)}
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

  def navigationHeader(meta: Option[Meta],
                       previous: Option[tree.Chapter],
                       next: Option[tree.Chapter]): web.tree.Node = {
    val previousHtml = previous.map { ch =>
      val href = s"${ch.id.get}.html"
      html"""<span>Previous chapter: <a href=$href>${ch.title}</a></span>"""
    }.getOrElse(
      html"""<a href="index.html">Table of contents</a>"""
    )

    val nextHtml = next.map { ch =>
      val href = s"${ch.id.get}.html"
      html"""<span>Next chapter: <a href=$href>${ch.title}</a></span>"""
    }.getOrElse(web.tree.Null)

    val separator =
      if (previousHtml != web.tree.Null && nextHtml != web.tree.Null) " | "
      else ""

    val title =
      meta.map { m =>
        html"""
          <header>
            <h1 class="title">${m.title}</h1>
          </header>
          """
      }.getOrElse(web.tree.Null)

    web.tree.PlaceholderSeqNode(Seq(
      title,
      html"<nav>$previousHtml $separator $nextHtml</nav>"))
  }

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
