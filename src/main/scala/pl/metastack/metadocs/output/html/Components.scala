package pl.metastack.metadocs.output.html

import org.joda.time.DateTime

import pl.metastack.metaweb._
import pl.metastack.{metaweb => web}

import pl.metastack.metadocs.output.HTML
import pl.metastack.metadocs.document.{tree, Meta}

object Components {
  def bodyWrapper(body: web.tree.Node) = {
    val generatedWith = Components.generatedWith()
    htmlT"""
    <div id="wrapper">
      $body
      $generatedWith
    </div>
    """
  }

  def generatedWith(smallClass: Boolean = false): web.tree.Node = {
    val link = htmlT"""<a href="http://github.com/MetaStack-pl/MetaDocs">MetaDocs</a>"""
    if (smallClass) htmlT"""<p class="small">Generated with $link</p>"""
    else htmlT"""<p><small>Generated with $link</small></p>"""
  }

  def copyright(meta: Meta): web.tree.Node = {
    val dt = new DateTime
    val year = dt.year().getAsText
    htmlT"""<p class="small">© Copyright $year ${meta.author}</p>"""
  }

  def toc(root: tree.Root,
          maxDepth: Int,
          referenceUrl: String => String): web.tree.Node = {
    def render(caption: String,
               id: Option[String],
               children: Seq[web.tree.Node]): web.tree.Node = {
      val childrenHtml = children.map(child => htmlT"<ul>$child</ul>")

      val url = id.map(referenceUrl)
      htmlT"<li><a href=$url>$caption</a>$childrenHtml</li>"
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

    if (toc.isEmpty) web.tree.Null
    else htmlT"""<nav id="toc"><ul>$toc</ul></nav>"""
  }

  def footnotes(writer: HTML,
                footnotes: Seq[tree.Footnote],
                hr: Boolean = true): web.tree.Node =
    if (footnotes.isEmpty) web.tree.Null
    else {
      val items = footnotes.map { fn =>
        val id = fn.id.get
        val fnId = s"fn$id"
        val target = s"#fnref$id"

        htmlT"""
            <li id=$fnId>
              <p>
                ${writer.children(fn)}
                <a href=$target class="reversefootnote">&#160;&#8617;</a>
              </p>
            </li>
          """
      }

      val hrT: web.tree.Node = if (hr) htmlT"<hr />" else web.tree.Null

      htmlT"""
          <div class="footnotes">
            $hrT
            <ol>$items</ol>
          </div>
        """
    }

  def header(meta: Option[Meta]): web.tree.Node = {
    meta.map { m =>
      val date = m.date.toString("MMMM YYYY", m.locale)

      htmlT"""
        <header>
          <h3 class="date">$date</h3>
          <h1 class="title">${m.title}</h1>
          <h2 class="author">${m.author}</h2>
          <p class="affiliation"><em>${m.affiliation}</em></p>
        </header>
      """
    }.getOrElse(web.tree.Null)
  }

  def `abstract`(meta: Option[Meta]): web.tree.Node =
    meta.map { m =>
      htmlT"""<p><small><strong>Abstract: </strong><em>${m.`abstract`}</em></small></p>"""
    }.getOrElse(web.tree.Null)

  def navigationHeader(meta: Option[Meta],
                       previous: Option[tree.Chapter],
                       next: Option[tree.Chapter],
                       sourcePath: Option[String]): web.tree.Node = {
    val previousHtml = previous.map { ch =>
      val href = s"${ch.id.get}.html"
      htmlT"""<span>Previous chapter: <a href=$href>${ch.title}</a></span>"""
    }.getOrElse(
      htmlT"""<a href="index.html">Table of contents</a>"""
    )

    val nextHtml: web.tree.Node = next.map { ch =>
      val href = s"${ch.id.get}.html"
      htmlT"""<span>Next chapter: <a href=$href>${ch.title}</a></span>"""
    }.getOrElse(web.tree.Null)

    val editHtml: web.tree.Node = meta.flatMap(_.editSourceURL).flatMap { edit =>
      sourcePath.map { sp =>
        val href = edit + sp
        htmlT"""<span><a href=$href>Edit source</a></span>"""
      }
    }.getOrElse(web.tree.Null)

    val items = Seq(previousHtml, nextHtml, editHtml)
      .filter(_ != web.tree.Null)
      .foldLeft(Seq.empty[web.tree.Node]) { case (acc, cur) =>
        acc match {
          case Nil => Seq(cur)
          case a => a ++ Seq(web.tree.Text(" | "), cur)
        }
      }

    val title =
      meta.map { m =>
        htmlT"""
          <header>
            <h1 class="title">${m.title}</h1>
          </header>
          """
      }.getOrElse(web.tree.Null)

    web.tree.Container(
      Seq(
        title,
        // TODO Allow htmlT"<nav>${web.tree.Container(items)}</nav>"
        web.tree.Tag("nav", events = Map.empty, children = items)))
  }

  type Skeleton = (Option[Meta], Option[String], web.tree.Node) => web.tree.Node

  def pageSkeleton(cssPaths: Seq[String] = Seq.empty,
                   jsPaths: Seq[String] = Seq.empty,
                   script: Option[String] = None,
                   favicon: Option[String] = None,
                   rss: Option[String] = None
                  )(meta: Option[Meta],
                    pageTitle: Option[String],
                    body: web.tree.Node): web.tree.Node = {
    val siteTitle = meta.map(_.title)
    val fullTitle = (siteTitle.toSeq ++ pageTitle.toSeq).mkString(" - ")

    val language = meta.map(_.language).getOrElse("en-GB")

    val faviconT: web.tree.Node =
      favicon.map { href =>
        htmlT"""<link rel="shortcut icon" href=$href />"""
      }.getOrElse(web.tree.Null)

    val rssT: web.tree.Node = rss.map { href =>
      htmlT"""<link rel="alternate" type="application/rss+xml" title=$siteTitle href=$href />"""
    }.getOrElse(web.tree.Null)

    val cssT = cssPaths.map { href =>
      htmlT"""<link rel="stylesheet" type="text/css" href=$href />"""
    }

    val jsPathsT = jsPaths.map { path =>
      htmlT"<script src=$path></script>"
    }

    val scriptT: web.tree.Node = script.map { js =>
      htmlT"<script>$js</script>"
    }.getOrElse(web.tree.Null)

    htmlT"""
      <!DOCTYPE html>
      <html lang="$language">
        <head>
          <title>$fullTitle</title>
          <meta charset="utf-8" />
          <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />
          <meta name="viewport" content="width=device-width, initial-scale=1.0" />
          <meta name="generator" content="MetaDocs" />
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
