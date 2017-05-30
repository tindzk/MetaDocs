package pl.metastack.metadocs.input.metadocs

import minitest._

import fastparse.core
import fastparse.core.Parsed

object ParserSpec extends SimpleTestSuite {
  import Parser._  // Import all parser rules

  def checkRule[T](rule: core.Parser[T, Char, String], input: String, output: T) {
    rule.parse(input) match {
      case s: Parsed.Success[T, Char, String] =>
        assertEquals(rule.parse(input).get.value, output)
      case f: Parsed.Failure[Char, String] => fail(f.msg)
    }
  }

  def shouldFail[T](rule: core.Parser[T, Char, String], input: String) {
    rule.parse(input) match {
      case s: Parsed.Success[T, Char, String] => fail()
      case f: Parsed.Failure[Char, String] =>
    }
  }

  test("identifierChar") {
    checkRule(identifier, "test", "test")
    checkRule(identifier, "test ", "test")
    checkRule(identifier, "test2", "test2")
    checkRule(identifier, "test2[", "test2")
    checkRule(identifier, "test2,", "test2")
    checkRule(identifier, "test2{", "test2")
    checkRule(identifier, "test2\t", "test2")
    checkRule(identifier, "test2=", "test2")
    checkRule(identifier, "test2\"", "test2")
    checkRule(identifier, "test?!%/", "test?!%/")
    checkRule(identifier, "äöü", "äöü")
  }

  test("argument") {
    checkRule(argument, "test", tree.Argument.Unnamed("test"))
    checkRule(argument, "test,", tree.Argument.Unnamed("test"))
    checkRule(argument, "k=v", tree.Argument.Named("k", "v"))
    checkRule(argument, "k=Hello world", tree.Argument.Named("k", "Hello world"))
    checkRule(argument, "k =Hello", tree.Argument.Unnamed("k "))
    checkRule(argument, """"Hello world,=!"""", tree.Argument.Unnamed("Hello world,=!"))
    checkRule(argument, """"Hello "world"""", tree.Argument.Unnamed("""Hello """))
    checkRule(argument, """"Hello \"world,=!"""", tree.Argument.Unnamed("""Hello "world,=!"""))
  }

  test("text") {
    checkRule(text, "Hello", tree.Text("Hello"))
    checkRule(text, "Hello!", tree.Text("Hello!"))
    checkRule(text, "Hello world![],.", tree.Text("Hello world![],."))
    checkRule(text, "Hello \"world![],.", tree.Text("Hello \"world![],."))
    checkRule(text, "äöü{test", tree.Text("äöü{test"))
    shouldFail(text, "äöü{test}")
    checkRule(text, "äöü{test\\}", tree.Text("äöü{test}"))
  }

  test("tag") {
    checkRule(tag, "tag{}", tree.Tag("tag"))
    checkRule(tag, "tag{*{}*}", tree.Tag("tag", children = Seq(tree.Text("{}"))))
    checkRule(tag, "tag{*(*)*}", tree.Tag("tag", children = Seq(tree.Text("(*)"))))
    checkRule(tag, "tag{*{\\*}*}", tree.Tag("tag", children = Seq(tree.Text("{*}"))))
    checkRule(tag, "tag[]{}", tree.Tag("tag"))
  }

  test("node") {
    checkRule(node, "tag{}", tree.Tag("tag"))
    checkRule(node, "tag {}", tree.Tag("tag"))
    checkRule(node, "text", tree.Text("text"))
    checkRule(node, "text[]", tree.Text("text[]"))
    checkRule(node, "tag{text}", tree.Tag("tag", children = Seq(tree.Text("text"))))
    checkRule(node, "tag[a,b]{text}", tree.Tag("tag",
      arguments = Seq(tree.Argument.Unnamed("a"), tree.Argument.Unnamed("b")),
      children = Seq(tree.Text("text"))))
    checkRule(node, "tag{a b{}}", tree.Tag("tag", children = Seq(
      tree.Text("a "), tree.Tag("b"))))
  }

  test("nodes") {
    checkRule(nodes, "", Seq.empty)
    checkRule(nodes, "tag{}tag2{}", Seq(tree.Tag("tag"), tree.Tag("tag2")))
    checkRule(nodes, "tag{}text", Seq(tree.Tag("tag"), tree.Text("text")))
    checkRule(nodes, "tag{} text", Seq(tree.Tag("tag"), tree.Text(" text")))
    checkRule(nodes, "text tag{}", Seq(tree.Text("text "), tree.Tag("tag")))
    checkRule(nodes, "(tag{a})", Seq(
      tree.Text("("),
      tree.Tag("tag", children = Seq(tree.Text("a"))),
      tree.Text(")")))
  }

  test("root") {
    checkRule(root, "tag{}tag2{}", tree.Root(Seq(tree.Tag("tag"), tree.Tag("tag2"))))
    checkRule(root, "", tree.Root(Seq.empty))
  }

  test("Error handling") {
    val error = Parser.parse("\ntag[{}").left.get
    assertEquals(error.position, "2:6")
  }
}
