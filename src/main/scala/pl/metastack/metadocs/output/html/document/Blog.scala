package pl.metastack.metadocs.output.html.document

import java.io.File

import org.joda.time.DateTime

import pl.metastack.metaweb._
import pl.metastack.{metaweb => web}

import pl.metastack.metadocs.output.HTML
import pl.metastack.metadocs.output.html.Components
import pl.metastack.metadocs.document.{Document, tree, Meta, Extractors}

object Blog {
  implicit def dateTimeOrdering: Ordering[DateTime] =
    Ordering.fromLessThan(_ isBefore _)

  /** Absolute post URL */
  def postUrl(meta: Meta, post: tree.Post): String =
    s"${meta.url}${post.id.get}.html"

  def postList(root: tree.Root, meta: Meta, referenceUrl: String => String): web.tree.Node = {
    def iterate(node: tree.Node): Option[tree.Post] =
      node match {
        case post: tree.Post => Some(post)
        case _               => None
      }

    val list = root.children.flatMap(iterate)
      .sortBy(_.date).reverse
      .map { case tree.Post(_, id, date, title, description, children @ _*) =>
        val url = id.map(referenceUrl)
        val descriptionT = description.map(d => html"<h2>$d</h2>").toSeq
        val dateFmt = date.toString("MMM dd", meta.locale)

        html"""
        <li>
          <a href=$url><aside class="dates">$dateFmt</aside></a>
          <a href=$url>$title $descriptionT</a>
        </li>
        """
      }

    html"""<ul id="post-list">$list</ul>"""
  }

  def index(root: tree.Root,
            meta: Meta,
            header: Option[web.tree.Node],
            bodyHeader: Option[web.tree.Node],
            footer: Option[web.tree.Node],
            referenceUrl: String => String): Seq[web.tree.Node] = {
    val headerT = header.toSeq
    val bodyHeaderT = bodyHeader.toSeq
    val content = postList(root, meta, referenceUrl)
    val footerT = footer.map { ft =>
      html"""<footer class="clearfix" id="footer">$ft</footer>"""
    }.toSeq

    headerT ++ Seq(
      html"""
      <div id="wrapper">
        $bodyHeaderT
        <section class="home">
          $content
          $footerT
        </section>
      </div>
      """
    )
  }

  def post(writer: HTML,
           meta: Meta,
           pageFooter: Option[web.tree.Node],
           header: Option[web.tree.Node],
           footer: Option[web.tree.Node],
           post: tree.Post): Seq[web.tree.Node] = {
    val dateFmt = post.date.toString("MMMM dd, YYYY", meta.locale)

    val avatarT = meta.avatar.map(src =>
      html"""<img class="avatar" src=$src />""").toSeq
    val headerT = header.toSeq
    val footerT = footer.toSeq
    val pageFooterT = pageFooter.toSeq

    val body =
      html"""
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
    val footnotesT = Components.footnotes(writer, footnotes, hr = false).toSeq

    headerT ++ Seq(
      html"""
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
    )
  }

  def feed(meta: Meta, posts: Seq[tree.Post]): web.tree.Tag = {
    def encodePost(post: tree.Post): web.tree.Tag = {
      val date = post.date.toString("E, d MMM yyyy HH:mm:ss Z", meta.locale)
      val url = postUrl(meta, post)
      html"""
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

    html"""<?xml version="1.0" encoding="UTF-8"?>
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
