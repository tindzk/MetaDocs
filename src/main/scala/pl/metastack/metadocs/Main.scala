package pl.metastack.metadocs

import java.io.File
import java.text.SimpleDateFormat

import io.circe.generic.auto._
import io.circe.parser._
import org.http4s.dsl._
import org.http4s.{server, _}
import org.http4s.server.blaze._
import org.joda.time.DateTime
import pl.metastack.metadocs.input.metadocs._
import pl.metastack.metadocs.output.html.document.SinglePage
import stoml.Toml.Elem
import stoml.TomlParserApi.TomlContent
import stoml.{Toml, TomlParserApi}

// TODO Add type checking to Toml, e.g. codec derivation
object TomlWrapper {
  def parseFile(file: File) =
    TomlParserApi.parseToml(file).get.value

  def lookUp(content: TomlContent, key: String) =
    content.lookup(key).get.asInstanceOf[Toml.Table]

  def lookUp[E <: Elem](node: Toml.Table, key: String): E#T =
    node.elem._2(key).asInstanceOf[E].elem

  def lookUpOpt[E <: Elem](node: Toml.Table, key: String): Option[E#T] =
    node.elem._2.get(key).map(_.asInstanceOf[E].elem)

  def exists(node: Toml.Table, key: String) =
    node.elem._2.contains(key)
}

class Project(file: File) {
  import Toml._

  val root = TomlWrapper.parseFile(file)
  val meta = TomlWrapper.lookUp(root, "meta")

  val format = new SimpleDateFormat("yyyy-MM-dd")

  val doc = document.Meta(
    date = new DateTime(
      TomlWrapper.lookUpOpt[Str](meta, "date")
      .map(format.parse)
      .getOrElse(DateTime.now())
    ),
    title = TomlWrapper.lookUp[Str](meta, "title"),
    author = TomlWrapper.lookUp[Str](meta, "author"),
    affiliation = TomlWrapper.lookUp[Str](meta, "affiliation"),
    `abstract` = TomlWrapper.lookUp[Str](meta, "abstract"),
    language = TomlWrapper.lookUp[Str](meta, "language"),
    url = TomlWrapper.lookUpOpt[Str](meta, "url").getOrElse(""),
    avatar = TomlWrapper.lookUpOpt[Str](meta, "avatar"),
    editSourceURL = TomlWrapper.lookUpOpt[Str](meta, "editSourceUrl")
  )

  val build = TomlWrapper.lookUp(root, "build")
  val paths = TomlWrapper.lookUp[Arr](build, "paths").map(_.elem)
    .asInstanceOf[Seq[String]]

  val constants = TomlWrapper.lookUp(root, "constants")
  val inherit = TomlWrapper.lookUpOpt[Str](constants, "inherit")

  val listings = TomlWrapper.lookUp(root, "listings")
  val listingsPath = TomlWrapper.lookUpOpt[Str](listings, "path")
}

object SbtParser {
  def parseConstants(file: File): Map[String, String] =
    FileUtils.readFile(file) { source =>
      val regex = """(\w+) in ThisBuild := "(.*)"""".r
      regex.findAllMatchIn(source.mkString).toList.map { x =>
        x.group(1) -> x.group(2)
      }.toMap
    }
}

sealed trait Action
object Action {
  case class Build(output: Option[File]) extends Action
  case class Serve(port: Int = 8080) extends Action
}

case class Options(action: Action = Action.Build(None),
                   watch: Boolean = false,
                   file: Option[File] = None)

object Builder {
  def resolvePath(path: String, projectDir: File): File = {
    val file = new File(path)
    if (file.isAbsolute) file else new File(projectDir, path)
  }

  def build(project: Project, projectDir: File): String = {
    val instructionSet = DefaultInstructionSet
      .inherit(BookInstructionSet)
      .inherit(CodeInstructionSet)
      .inherit(DraftInstructionSet)
      .withAliases("b" -> Bold, "i" -> Italic, "li" -> ListItem)

    val constants = project.inherit.map { path =>
      SbtParser.parseConstants(resolvePath(path, projectDir))
    }.getOrElse(Map.empty[String, String])

    val rawTrees = project.paths.map(path =>
      input.Markdown.loadFileWithExtensions(resolvePath(path, projectDir), path,
        instructionSet,
        constants = constants,
        generateId = caption => Some(caption.collect {
          case c if c.isLetterOrDigit => c
          case c if c.isSpaceChar => '-'
        }.toLowerCase)
      ).get)

    val docTree = document.Document.mergeTrees(rawTrees)

    // Explicitly print out all chapters/sections which is useful when
    // restructuring the document
    println("Document tree:")
    println(document.Extractors.references(docTree))
    println()

    document.Document.printTodos(docTree)

    val blocks = project.listingsPath.map { path =>
      FileUtils.readFile(resolvePath(path, projectDir)) { json =>
        decode[Map[String, BlockResult]](json.mkString) match {
          case Left(_) => throw new Exception(s"Cannot read listing file $path")
          case Right(s) => s
        }
      }
    }

    val pipeline = blocks match {
      case None => document.Document.pipeline
      case Some(b) =>
        document.Document.pipeline
          .andThen(document.CodeProcessor.embedListings(b) _)
    }

    val docTreeWithCode = pipeline(docTree)

    val skeleton = output.html.Components.pageSkeleton(
      cssPaths = Seq(
        "http://metastack.pl/css/kult.css",
        "http://metastack.pl/css/default.min.css"
      ),
      jsPaths = Seq(
        "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js",
        "http://metastack.pl/js/main.js",
        "http://metastack.pl/js/highlight.pack.js"
      ),
      script = Some("hljs.initHighlightingOnLoad();"),
      favicon = Some("http://metastack.pl/favicon.ico")
    )(_, _, _)

    val result = SinglePage.compose(docTreeWithCode,
      skeleton,
      meta = Some(project.doc),
      toc = true,
      tocDepth = 2) // Don't include subsections

    result.toHtml
  }
}

object Main {
  val parser = new scopt.OptionParser[Options]("metadocs") {
    head("metadocs", BuildInfo.version)

    opt[Unit]("watch").action((_, c) =>
      c.copy(watch = true)).text("watch for file changes")

    note("")

    cmd("build").action((_, c) => c.copy(action = Action.Build(None)))
      .children(
        opt[File]('o', "out").valueName("<file>")
          .action((x, c) => c.copy(action = Action.Build(Some(x))))
          .text("output file (default: <project>.html)")
      )
      .text("build project")

    note("")

    cmd("serve").action((_, c) => c.copy(action = Action.Serve()))
      .text("start HTTP server")
      .children(
        opt[Int]("port").action((x, c) =>
          c.copy(action = Action.Serve(x))).text("listen on port (default = 8080)")
      )

    note("")

    arg[File]("<file>.toml").action((x, c) => c.copy(file = Some(x)))
      .text("project file")
  }

  def build(projectFile: File, outputFile: File): Unit = {
    val project = new Project(projectFile)
    val projectDir = projectFile.getAbsoluteFile.getParentFile
    val result = Builder.build(project, projectDir)
    FileUtils.printToFile(outputFile)(_.write(result))
  }

  def serve(projectFile: File, port: Int): Unit = {
    val project = new Project(projectFile)
    val projectDir = projectFile.getAbsoluteFile.getParentFile
    val result = Builder.build(project, projectDir)
    val service = HttpService {
      case GET -> Root =>
        Ok(result).withContentType(Some(MediaType.`text/html`))
    }

    val srv = new server.ServerApp {
      override def server(args: List[String]) =
        BlazeBuilder
          .bindHttp(port, "localhost")
          .mountService(service)
          .start
    }

    srv.main(Array.empty)
  }

  def main(args: Array[String]): Unit =
    parser.parse(args, Options()) match {
      case Some(config) =>
        config.action match {
          case Action.Build(output) => build(config.file.get,
            output.getOrElse(
              new File(FileUtils.stripTomlExtension(
                config.file.get.getAbsolutePath) + ".html")))
          case Action.Serve(port) => serve(config.file.get, port)
        }

        System.exit(0)

      case None => System.exit(1)
    }
}
