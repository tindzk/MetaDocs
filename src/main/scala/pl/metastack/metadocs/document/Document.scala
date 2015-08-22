package pl.metastack.metadocs.document

import java.io.File

import pl.metastack.metadocs._
import pl.metastack.metadocs.input.InstructionSet
import pl.metastack.metadocs.input.tree.Root

import scala.util.{Failure, Success, Try}

object Document {
  def loadFile(filePath: String): Try[Root] = {
    val contents = io.Source.fromFile(new File(filePath)).mkString
    input.Parser.parse(contents)
  }

  /** Merge trees of loaded files */
  def loadFiles(filePaths: Seq[String]): Root =
    Root(
      filePaths.flatMap { filePath =>
        loadFile(filePath) match {
          case Success(root) => root.children
          case Failure(error) =>
            println(s"File $filePath could not be parsed:")
            println(error)
            Seq.empty
        }
      }
    )

  /**
   * @param generateId If the ID of a structural element (chapter, section etc.)
   *                   is missing, this function will be called with the caption.
   */
  def toDocumentTree(root: Root,
                     instructionSet: InstructionSet,
                     generateId: String => Option[String] = _ => None): tree.Root = {
    val conversion = new input.Conversion(instructionSet, generateId)
    conversion.convertRoot(root)
  }

  def generateTOC(root: tree.Root, maxLevel: Int): TableOfContents = {
    // TODO Verify that IDs in TOC are unique
    def heading(id: Option[String],
                caption: String,
                children: Seq[tree.Node], level: Int) =
      Some(
        Heading(
          caption = caption,
          id = id,
          children =
            if (level + 1 < maxLevel) children.flatMap(iterate(_, level + 1))
            else Seq.empty
        )
      )

    def iterate(node: tree.Node, level: Int = 0): Option[Heading] =
      node match {
        case tag @ tree.Chapter(id, caption, children @ _*) =>
          heading(id, caption, children, level)
        case tag @ tree.Section(id, caption, children @ _*) =>
          heading(id, caption, children, level)
        case tag @ tree.Subsection(id, caption, children @ _*) =>
          heading(id, caption, children, level)
        case _ => None
      }

    TableOfContents(root.children.flatMap(iterate(_)))
  }

  def printTodos(root: document.tree.Root) {
    def iterate(node: document.tree.Node) {
      node match {
        case node @ document.tree.Todo(todo) => println(s"[todo] ${todo.text}")
        case _ => node.children.foreach(iterate)
      }
    }

    iterate(root)
  }
}
