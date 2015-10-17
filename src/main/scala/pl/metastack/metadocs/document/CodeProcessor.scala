package pl.metastack.metadocs.document

import java.io.File

import scala.util.Try
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

import pl.metastack.metadocs.{TextHelpers, SectionSupport}
import pl.metastack.metadocs.document.tree.ScalaType

/**
 * @author Matt Hicks <matt@outr.com>
 * @author Tim Nieradzik <tim@metastack.pl>
 */
object CodeProcessor {
  def scalaFile(`package`: Option[String], fileName: String): File = {
    val packagePath = `package`.getOrElse("").replaceAll("[.]", "/")
    new File(s"src/main/scala/$packagePath/$fileName.scala")
  }

  private def forLines(file: File)(f: String => Unit): Unit = {
    val source = io.Source.fromFile(file)
    try {
      source.getLines().foreach(f)
    } finally {
      source.close()
    }
  }

  private def forBlock(file: File,
                       isStart: String => Boolean,
                       isEnd: String => Boolean,
                       process: String => Option[String] = (s: String) => Some(s),
                       includeStart: Boolean = true,
                       includeEnd: Boolean = true): List[String] = {
    var started = false
    var ended = false
    val lines = ListBuffer.empty[String]

    def proc(line: String) = process(line) match {
      case Some(l) => lines += l
      case None =>  // Ignore
    }

    var lineNumber = 0
    forLines(file) { line =>
      try {
        if (!ended && !started && isStart(line)) {
          started = true
          if (includeStart) proc(line)
        } else if (!ended && started && isEnd(line)) {
          ended = true
          if (includeEnd) proc(line)
        } else if (started && !ended) {
          proc(line)
        }
      } catch {
        case t: Throwable =>
          throw new RuntimeException(s"Unable to process line: [$line] (line number: $lineNumber)", t)
      }

      lineNumber += 1
    }

    lines.toList
  }

  def imports(file: File): List[String] =
    forBlock(
      file,
      (s: String) => s.startsWith("import"),
      (s: String) => s.startsWith("class") || s.startsWith("object"),
      (s: String) => if (s.startsWith("import")) Some(s) else None,
      includeEnd = false)

  private def trimmingProcessor(length: => Int): String => Option[String] =
    (line: String) =>
      Some(
        if (line.length > length) line.substring(length)
        else line)

  def handleObject(file: File, fileName: String) = {
    var spacing = ""
    forBlock(file,
      (s: String) => {
        val b = s.trim.startsWith(s"object $fileName")
        if (b) spacing = s.substring(0, s.indexOf('o'))
        b
      },
      (s: String) => s == s"$spacing}",
      trimmingProcessor(spacing.length)
    )
  }

  def handleClass(file: File, fileName: String): List[String] = {
    var spacing = ""
    forBlock(
      file,
      (s: String) => {
        val b = s.trim.startsWith(s"class $fileName")
        if (b) spacing = s.substring(0, s.indexOf('c'))
        b
      },
      (s: String) => s == s"$spacing}",
      trimmingProcessor(spacing.length))
  }

  def handleCaseClass(file: File, fileName: String): List[String] = {
    var spacing = ""
    forBlock(
      file,
      (s: String) => {
        val b = s.trim.startsWith(s"case class $fileName")
        if (b) spacing = s.substring(0, s.indexOf('c'))
        b
      },
      (s: String) => s == s"$spacing}",
      trimmingProcessor(spacing.length))
  }

  def handleTrait(file: File, fileName: String): List[String] = {
    var spacing = ""
    forBlock(
      file,
      (s: String) => {
        val b = s.trim.startsWith(s"trait $fileName")
        if (b) spacing = s.substring(0, s.indexOf('t'))
        b
      },
      (s: String) => s == s"$spacing}",
      trimmingProcessor(spacing.length))
  }

  def section(file: File, sectionName: String): List[String] = {
    var spacing = ""
    var length = -1

    val trimmer = (line: String) => {
      val s = if (length == -1 && line.trim.startsWith("section")) {
        line.substring(spacing.length)
      } else if (length != -1 && line == s"$spacing}") {
        line.trim
      } else {
        if (length == -1) {
          val Regex = "(\\W*)(.+)".r
          line match {
            case Regex(whitespace, other) => length = whitespace.length
          }
        }

        if (line.length > length) line.substring(length)
        else line
      }

      Some(s)
    }

    forBlock(
      file,
      isStart = (s: String) => {
        val b = s.contains(s"""section("$sectionName"""") ||
          s.contains(s"""sectionNoExec("$sectionName"""")
        if (b) {
          @tailrec
          def countWhitespace(count: Int, s: String): Int = {
            if (!s.charAt(0).isWhitespace) count
            else countWhitespace(count + 1, s.substring(1))
          }

          val length = countWhitespace(0, s)
          spacing = s.substring(0, length)
        }

        b
      },
      isEnd = (s: String) => s == s"$spacing}",
      trimmer,
      includeStart = false,
      includeEnd = false
    )
  }

  def embedListings(root: tree.Root): tree.Root = {
    var `package` = Option.empty[String]

    def iterate(node: tree.Node): tree.Node =
      node match {
        case pkg: tree.Package =>
          `package` = Some(pkg.value)
          pkg

        case scala: tree.Scala =>
          lazy val fileName = scala.file.getOrElse(
            throw new RuntimeException(s"Scala listing $scala doesn't set a filename"))
          lazy val file = scalaFile(`package`, fileName)
          val code = scala.`type` match {
            case ScalaType.Code => scala.code.get
            case ScalaType.Imports => imports(file).mkString("\n")
            case ScalaType.Object => handleObject(file, fileName).mkString("\n")
            case ScalaType.Class => handleClass(file, fileName).mkString("\n")
            case ScalaType.CaseClass => handleCaseClass(file, fileName).mkString("\n")
            case ScalaType.Trait => handleTrait(file, fileName).mkString("\n")
            case ScalaType.Section => section(file, scala.value).mkString("\n")
          }
          scala.copy(code = Some(TextHelpers.reindent(code)))

        case node: tree.Node => node
      }

    root.map(iterate).asInstanceOf[tree.Root]
  }

  def runSection(`package`: Option[String],
                 fileName: String,
                 sectionName: String): Option[Any] = {
    val className = `package`.map(_ + ".").getOrElse("") + fileName
    val classLoader = getClass.getClassLoader
    val companionClass = Try(Class.forName(className + "$", true, classLoader))
      .getOrElse(throw new RuntimeException(s"No companion object for $className"))
    val companion = companionClass.getField("MODULE$")
      .get(null)
      .asInstanceOf[SectionSupport]
    companion.sectionResult(sectionName)
  }

  /** Execute listings and embed their output */
  def embedOutput(root: tree.Root): tree.Root = {
    var `package` = Option.empty[String]

    root.map {
      case pkg: tree.Package =>
        `package` = Some(pkg.value)
        pkg

      case scala: tree.Scala =>
        val result = scala.`type` match {
          case ScalaType.Section =>
            val fileName = scala.file.getOrElse(
              throw new RuntimeException(s"Scala listing $scala doesn't set a filename")
            )
            println(s"Executing listing: ${scala.value}")
            runSection(`package`, fileName, scala.value).map(_.toString)

          case _ => None
        }

        scala.copy(result = result)

      case node: tree.Node => node
    }.asInstanceOf[tree.Root]
  }
}
