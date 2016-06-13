package pl.metastack.metadocs.output.html

import org.joda.time.DateTime

import pl.metastack.metaweb._
import pl.metastack.{metaweb => web}

import pl.metastack.metadocs.BuildInfo
import pl.metastack.metadocs.output.HTML
import pl.metastack.metadocs.document.{tree, Meta}

object Components {
  def bodyWrapper(body: Seq[web.tree.Node]): web.tree.Tag = {
    val generatedWith = Components.generatedWith()
    html"""
    <div id="wrapper">
      $body
      $generatedWith
    </div>
    """
  }

  def generatedWith(smallClass: Boolean = false): web.tree.Node = {
    val caption = "MetaDocs v" + BuildInfo.version
    val link = html"""<a href="http://github.com/MetaStack-pl/MetaDocs">$caption</a>"""
    if (smallClass) html"""<p class="small">Generated with $link</p>"""
    else html"""<p><small>Generated with $link</small></p>"""
  }

  def copyright(meta: Meta): web.tree.Node = {
    val dt = new DateTime
    val year = dt.year().getAsText
    html"""<p class="small">© Copyright $year ${meta.author}</p>"""
  }

  def toc(root: tree.Root,
          maxDepth: Int,
          referenceUrl: String => String): Option[web.tree.Node] = {
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
        case tag @ tree.Chapter(_, id, caption, children @ _*) =>
          Some(render(caption, id, children.flatMap(iterate(_, depth + 1))))
        case tag @ tree.Section(id, caption, children @ _*) =>
          Some(render(caption, id, children.flatMap(iterate(_, depth + 1))))
        case tag @ tree.Subsection(id, caption, children @ _*) =>
          Some(render(caption, id, children.flatMap(iterate(_, depth + 1))))
        case _ => None
      }

    val toc = root.children.flatMap(iterate(_, 0))

    if (toc.isEmpty) None
    else Some(html"""<nav id="toc"><ul>$toc</ul></nav>""")
  }

  def footnotes(writer: HTML,
                footnotes: Seq[tree.Footnote],
                hr: Boolean = true): Option[web.tree.Node] =
    if (footnotes.isEmpty) None
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

      val hrT = if (hr) Some(html"<hr />") else None

      Some(
        html"""
            <div class="footnotes">
              ${hrT.toSeq}
              <ol>$items</ol>
            </div>
          """
      )
    }

  def header(meta: Option[Meta]): Option[web.tree.Node] = {
    meta.map { m =>
      val date = m.date.toString("MMMM YYYY", m.locale)

      html"""
        <header>
          <h3 class="date">$date</h3>
          <h1 class="title">${m.title}</h1>
          <h2 class="author">${m.author}</h2>
          <p class="affiliation"><em>${m.affiliation}</em></p>
        </header>
      """
    }
  }

  def `abstract`(meta: Option[Meta]): Option[web.tree.Node] =
    meta.map { m =>
      html"""<p><small><strong>Abstract: </strong><em>${m.`abstract`}</em></small></p>"""
    }

  def navigationHeader(meta: Option[Meta],
                       previous: Option[tree.Chapter],
                       next: Option[tree.Chapter],
                       sourcePath: Option[String]): Seq[web.tree.Node] = {
    val previousHtml = previous.map { ch =>
      val href = s"${ch.id.get}.html"
      html"""<span>Previous chapter: <a href=$href>${ch.title}</a></span>"""
    }.getOrElse(
      html"""<a href="index.html">Table of contents</a>"""
    )

    val nextHtml = next.map { ch =>
      val href = s"${ch.id.get}.html"
      html"""<span>Next chapter: <a href=$href>${ch.title}</a></span>"""
    }

    val editHtml = meta.flatMap(_.editSourceURL).flatMap { edit =>
      sourcePath.map { sp =>
        val href = edit + sp
        html"""<span><a href=$href>Edit source</a></span>"""
      }
    }

    val items = Seq(Some(previousHtml), nextHtml, editHtml)
      .collect { case Some(node) => node }
      .foldLeft(Seq.empty[web.tree.Node]) { case (acc, cur) =>
        acc match {
          case Nil => Seq(cur)
          case a => a ++ Seq(web.tree.Text(" | "), cur)
        }
      }

    val title =
      meta.map { m =>
        html"""
          <header>
            <h1 class="title">${m.title}</h1>
          </header>
          """
      }

    // TODO html"<nav>$items</nav>"
    title.toSeq ++ Seq(tag.Nav(children = items))
  }

  type Skeleton = (Option[Meta], Option[String], Seq[web.tree.Node]) => web.tree.Node

  def pageSkeleton(cssPaths: Seq[String] = Seq.empty,
                   jsPaths: Seq[String] = Seq.empty,
                   script: Option[String] = None,
                   favicon: Option[String] = None,
                   rss: Option[String] = None
                  )(meta: Option[Meta],
                    pageTitle: Option[String],
                    body: Seq[web.tree.Node]): web.tree.Node = {
    val siteTitle = meta.map(_.title)
    val fullTitle = (siteTitle.toSeq ++ pageTitle.toSeq).mkString(" - ")

    val language = meta.map(_.language).getOrElse("en-GB")

    val faviconT =
      favicon.map { href =>
        html"""<link rel="shortcut icon" href=$href />"""
      }.toSeq

    val rssT = rss.map { href =>
      html"""<link rel="alternate" type="application/rss+xml" title=$siteTitle href=$href />"""
    }.toSeq

    val cssT = cssPaths.map { href =>
      html"""<link rel="stylesheet" type="text/css" href=$href />"""
    }

    val jsPathsT = jsPaths.map { path =>
      html"<script src=$path></script>"
    }

    val scriptT = script.map { js =>
      html"<script>$js</script>"
    }.toSeq

    val generator = "MetaDocs v" + BuildInfo.version

    html"""
      <!DOCTYPE html>
      <html lang="$language">
        <head>
          <title>$fullTitle</title>
          <meta charset="utf-8" />
          <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />
          <meta name="viewport" content="width=device-width, initial-scale=1.0" />
          <meta name="generator" content=$generator />
          $faviconT
          $rssT
          $cssT
        </head>

        <body>
          $body
          $jsPathsT
          $scriptT
        </body>
      </html>
      """
    }
}
