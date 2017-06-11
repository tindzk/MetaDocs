package pl.metastack.metadocs.document

import java.util.Locale

import org.joda.time.DateTime

case class Meta(
  date: DateTime,  // TODO Should be optional, unless blog
  title: String,
  author: String,
  affiliation: String,
  `abstract`: String,

  /* Language code, e.g. en_GB */
  language: String,

  /* URL with trailing slash */
  url: String,  // TODO Should be optional

  /* URL to avatar */
  avatar: Option[String] = None,

  /* E.g. https://github.com/$user/$project/edit/master/ */
  editSourceURL: Option[String] = None
) {
  val locale = Locale.forLanguageTag(language)
}
