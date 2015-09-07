package pl.metastack.metadocs.input.markdown

import minitest.SimpleTestSuite

import pl.metastack.metadocs.document.tree._

object PegdownSpec extends SimpleTestSuite {
  test("Bold") {
    assertEquals(Pegdown.parse("**Hello**"),
      Root(Paragraph(Bold(Text("Hello")))))
  }

  test("Code") {
    assertEquals(Pegdown.parse("`code`"),
      Root(Paragraph(Code(Text("code")))))
    assertEquals(Pegdown.parse("``code``"),
      Root(Paragraph(Code(Text("code")))))
  }

  test("Link") {
    assertEquals(Pegdown.parse("[Google](http://google.com/)"),
      Root(Paragraph(Url("http://google.com/", Text("Google")))))
  }

  test("Source code") {
    assertEquals(Pegdown.parse(
      """
        |```scala
        |test()
        |```
      """.stripMargin),

      Root(
        Scala(code = "test()")
      )
    )
  }

  test("List") {
    assertEquals(Pegdown.parse(
      """
        |* Item 1
        |* Item 2
      """.stripMargin),

      Root(
        List(
          ListItem(Text("Item 1")),
          ListItem(Text("Item 2"))
        )
      )
    )
  }

  test("Chapters") {
    assertEquals(Pegdown.parse(
      """
        |# Chapter 1
        |Content 1
        |# Chapter 2
        |Content 2
      """.stripMargin),

      Root(
        Chapter(None, "Chapter 1", Paragraph(Text("Content 1"))),
        Chapter(None, "Chapter 2", Paragraph(Text("Content 2")))
      )
    )
  }

  test("Chapters with subsections") {
    assertEquals(Pegdown.parse(
      """
        |# Chapter 1
        |Content 1
        |## Section 1
        |Subcontent 1
        |## Section 2
        |Subcontent 2
        |# Chapter 2
        |Content 2
      """.stripMargin),

      Root(
        Chapter(None, "Chapter 1",
          Paragraph(Text("Content 1")),
          Section(None, "Section 1", Paragraph(Text("Subcontent 1"))),
          Section(None, "Section 2", Paragraph(Text("Subcontent 2")))
        ),
        Chapter(None, "Chapter 2", Paragraph(Text("Content 2")))
      )
    )
  }
}
