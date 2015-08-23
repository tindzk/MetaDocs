package pl.metastack.metadocs.document

import java.io.File

import pl.metastack.metadocs._
import pl.metastack.metadocs.input.InstructionSet
import pl.metastack.metadocs.input.tree.Root

import scala.collection.mutable
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

  def uniqueIds(root: tree.Root): tree.Root = {
    val collectedIds = mutable.HashMap.empty[String, Int]

    def rename(optId: Option[String]): Option[String] =
      optId.map { id =>
        if (!collectedIds.isDefinedAt(id)) {
          collectedIds += (id -> 0)
          id
        } else {
          collectedIds(id) += 1
          val renamedId = id + "-" + collectedIds(id)
          println(s"[warn] ID '$id' exists already, renamed to '$renamedId'")
          renamedId
        }
      }

    root.map {
      case tag @ tree.Chapter(id, caption, children @ _*) =>
        tag.copy(id = rename(id))
      case tag @ tree.Section(id, caption, children @ _*) =>
        tag.copy(id = rename(id))
      case tag @ tree.Subsection(id, caption, children @ _*) =>
        tag.copy(id = rename(id))
      case tag => tag
    }.asInstanceOf[tree.Root]
  }

  def validateJumps(root: tree.Root): tree.Root = {
    val idToCaption = mutable.HashMap.empty[String, String]

    root.map {
      case tag @ tree.Chapter(Some(id), caption, children @ _*) =>
        idToCaption += (id -> caption)
        tag
      case tag @ tree.Section(Some(id), caption, children @ _*) =>
        idToCaption += (id -> caption)
        tag
      case tag @ tree.Subsection(Some(id), caption, children @ _*) =>
        idToCaption += (id -> caption)
        tag
      case tag => tag
    }

    root.map {
      case tag @ tree.Jump(ref, caption) =>
        if (!idToCaption.contains(ref)) {
          println(s"[error] Invalid reference: $ref")
          tree.Text(s"[$ref (invalid reference)]")
        } else if (caption.isEmpty) {
          tag.copy(caption = Some(idToCaption(ref)))
        } else tag

      case tag => tag
    }.asInstanceOf[tree.Root]
  }

  def footnoteIds(root: tree.Root): tree.Root = {
    var lastId = 0
    root.map {
      case tag @ tree.Footnote(id, children @ _*) =>
        lastId += 1
        tag.copy(Some(lastId))
      case tag => tag
    }.asInstanceOf[tree.Root]
  }

  /**
   * @param generateId If the ID of a structural element (chapter, section etc.)
   *                   is missing, this function will be called with the caption.
   */
  def toDocumentTree(root: Root,
                     instructionSet: InstructionSet,
                     generateId: String => Option[String] = _ => None): tree.Root = {
    val conversion = new input.Conversion(instructionSet, generateId)
    val pipeline =
      (conversion.convertRoot _)
        .andThen(uniqueIds)
        .andThen(validateJumps)
        .andThen(footnoteIds)

    pipeline(root)
  }

  def generateTOC(root: tree.Root, maxLevel: Int): TableOfContents = {
    def heading(id: Option[String],
                caption: String,
                children: Seq[tree.Node],
                level: Int) =
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
