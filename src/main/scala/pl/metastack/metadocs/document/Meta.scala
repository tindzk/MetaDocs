package pl.metastack.metadocs.document

import java.util.Locale

import org.joda.time.DateTime

case class Meta(date: DateTime,
                title: String,
                author: String,
                affiliation: String,
                `abstract`: String,
                language: String,
                url: String) {
  val locale = Locale.forLanguageTag(language)
}
