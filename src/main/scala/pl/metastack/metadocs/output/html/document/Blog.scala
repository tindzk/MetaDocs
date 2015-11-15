package pl.metastack.metadocs.output.html.document

import java.io.File

import org.joda.time.DateTime

import pl.metastack.metaweb._
import pl.metastack.{metaweb => web}

import pl.metastack.metadocs.output.HTML
import pl.metastack.metadocs.output.html.Components
import pl.metastack.metadocs.document.{Document, tree, Meta, Extractors}

object Blog {
  /** Absolute post URL */
  def postUrl(meta: Meta, post: tree.Post): String =
    s"${meta.url}${post.id.get}.html"

  def postList(root: tree.Root, meta: Meta, referenceUrl: String => String): web.tree.Node = {
    def iterate(node: tree.Node): Option[web.tree.Node] =
      node match {
        case tag @ tree.Post(_, id, date, title, description, children @ _*) =>
          val url = id.map(referenceUrl)
          val descriptionT: web.tree.Node =
            description.map(d => htmlT"<h2>$d</h2>")
              .getOrElse(web.tree.Null)
          val dateFmt = date.toString("MMM dd", meta.locale)

          Some(
            htmlT"""
            <li>
              <a href=$url><aside class="dates">$dateFmt</aside></a>
              <a href=$url>$title $descriptionT</a>
            </li>
            """
          )
        case _ => None
      }

    val list = root.children.flatMap(iterate)
    htmlT"""<ul id="post-list">$list</ul>"""
  }

  def index(root: tree.Root,
            meta: Meta,
            header: Option[web.tree.Node],
            bodyHeader: Option[web.tree.Node],
            footer: Option[web.tree.Node],
            referenceUrl: String => String): web.tree.Node = {
    val headerT: web.tree.Node = header.getOrElse(web.tree.Null)
    val bodyHeaderT: web.tree.Node = bodyHeader.getOrElse(web.tree.Null)
    val content = postList(root, meta, referenceUrl)
    val footerT: web.tree.Node = footer.map { ft =>
      htmlT"""<footer class="clearfix" id="footer">$ft</footer>"""
    }.getOrElse(web.tree.Null)

    web.tree.Container(Seq(
      headerT,
      htmlT"""
      <div id="wrapper">
        $bodyHeaderT
        <section class="home">
          $content
          $footerT
        </section>
      </div>
      """
    ))
  }

  def post(writer: HTML,
           meta: Meta,
           pageFooter: Option[web.tree.Node],
           header: Option[web.tree.Node],
           footer: Option[web.tree.Node],
           post: tree.Post): web.tree.Node = {
    val dateFmt = post.date.toString("MMMM MM, YYYY", meta.locale)

    val avatarT: web.tree.Node = meta.avatar.map(src =>
      htmlT"""<img class="avatar" src=$src />""").getOrElse(web.tree.Null)
    val headerT = header.getOrElse(web.tree.Null)
    val footerT = footer.getOrElse(web.tree.Null)
    val pageFooterT: web.tree.Node = pageFooter.getOrElse(web.tree.Null)

    val body =
      htmlT"""
      <article class="post">
        <header>
          <h1>${post.title}</h1>
          <h2 class="headline">$dateFmt</h2>
        </header>
        <section id="post-body">
          ${writer.children(post)}
        </section>
      </article>
      """

    val footnotes = Extractors.footnotes(post)
    val footnotesT = Components.footnotes(writer, footnotes, hr = false)

    web.tree.Container(Seq(
      headerT,
      htmlT"""
      <section id="wrapper" class="home">
        $body
        $footnotesT
        <footer id="post-meta" class="clearfix">
          $avatarT
          <div>
            <span class="dark">${meta.author}</span>
            <span>${meta.`abstract`}</span>
          </div>
          $footerT
        </footer>
        $pageFooterT
      </section>
      """
    ))
  }

  def feed(meta: Meta, posts: Seq[tree.Post]): web.tree.Tag = {
    def encodePost(post: tree.Post): web.tree.Tag = {
      val date = post.date.toString("E, d MMM yyyy HH:mm:ss Z", meta.locale)
      val url = postUrl(meta, post)
      htmlT"""
        <item>
          <title>${post.title}</title>
          <description>${post.description.getOrElse("")}</description>
          <pubDate>$date</pubDate>
          <link>$url</link>
          <guid isPermaLink="true">$url</guid>
        </item>
      """
    }

    val encodedPosts: Seq[web.tree.Tag] = posts.take(15).map(encodePost)

    val feedUrl = s"${meta.url}posts.xml"

    htmlT"""<?xml version="1.0" encoding="UTF-8"?>
      <rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom" xmlns:dc="http://purl.org/dc/elements/1.1/">
        <channel>
          <title>${meta.title}</title>
          <description>${meta.`abstract`}</description>
          <link>${meta.url}</link>
          <atom:link href=$feedUrl rel="self" type="application/rss+xml" />
          <dc:creator>${meta.author}</dc:creator>
          <dc:language>${meta.language}</dc:language>
          $encodedPosts
        </channel>
      </rss>
    """
  }

  def write(root: tree.Root,
            skeleton: Components.Skeleton,
            pageFooter: Option[web.tree.Node],
            indexHeader: Option[web.tree.Node],
            indexBodyHeader: Option[web.tree.Node],
            postHeader: Option[web.tree.Node],
            postFooter: Option[tree.Post => web.tree.Node],
            outputPath: String,
            meta: Meta) {
    val filePath = new File(outputPath)
    filePath.mkdirs()

    val references = Extractors.references(root)

    def referenceUrl(id: String): String = {
      val resolved = references.resolve(id)
      val post = references.topLevelReferenceOf(resolved)
      val anchor =
        if (post == resolved) ""
        else s"#${resolved.id.get}"  // Reference to a section
      s"${post.id.get}.html$anchor"
    }

    val writer = new HTML(referenceUrl)

    val indexBody = index(root, meta, indexHeader, indexBodyHeader, pageFooter,
      referenceUrl)
    val indexResult = skeleton(Some(meta), None, indexBody)
    Document.writeHtml(filePath, "index", indexResult)

    implicit def dateTimeOrdering: Ordering[DateTime] =
      Ordering.fromLessThan(_ isBefore _)
    val posts = Extractors.posts(root).sortBy(_.date).reverse

    posts.foreach { p =>
      val body = post(writer, meta, pageFooter, postHeader, postFooter.map(_(p)), p)
      val result = skeleton(Some(meta), Some(p.title), body)
      Document.writeHtml(filePath, p.id.get, result)
    }

    val feedResult = feed(meta, posts)
    Document.writeXml(filePath, "posts", feedResult)
  }
}
