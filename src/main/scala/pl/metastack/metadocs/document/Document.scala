package pl.metastack.metadocs.document

import java.io.File

import scala.collection.mutable

import pl.metastack.{metaweb => web}

import pl.metastack.metadocs._

object Document {
  /** Merge all trees and add source path reference to each top-level node */
  def mergeTrees(trees: Seq[tree.Root]): tree.Root = {
    val children = trees.flatMap { t =>
      t.map {
        case tag: tree.Post => tag.copy(sourcePath = t.sourcePath)
        case tag: tree.Chapter => tag.copy(sourcePath = t.sourcePath)
        case tag => tag
      }.children
    }

    tree.Root(None, children: _*)
  }

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
      case tag: tree.Post => tag.copy(id = rename(tag.id))
      case tag: tree.Chapter => tag.copy(id = rename(tag.id))
      case tag: tree.Section => tag.copy(id = rename(tag.id))
      case tag: tree.Subsection => tag.copy(id = rename(tag.id))
      case tag => tag
    }.asInstanceOf[tree.Root]
  }

  def validateJumps(root: tree.Root): tree.Root = {
    val idToCaption = mutable.HashMap.empty[String, String]

    root.map {
      case tag @ tree.Post(_, Some(id), _, caption, _, children @ _*) =>
        idToCaption += (id -> caption)
        tag
      case tag @ tree.Chapter(_, Some(id), caption, children @ _*) =>
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

  val pipeline =
    (uniqueIds _)
      .andThen(validateJumps)
      .andThen(footnoteIds)

  def printTodos(root: document.tree.Root) {
    def iterate(node: document.tree.Node) {
      node match {
        case node @ document.tree.Todo(todo) => println(s"[todo] ${todo.text}")
        case _ => node.children.foreach(iterate)
      }
    }

    iterate(root)
  }

  def writeHtml(filePath: File, id: String, root: web.tree.Node) {
    import pl.metastack.metaweb._
    FileUtils.printToFile(new File(filePath, s"$id.html")) { fw =>
      fw.write(root.state.toHtml)
    }
  }

  def writeXml(filePath: File, id: String, root: web.tree.Node) {
    import pl.metastack.metaweb._
    FileUtils.printToFile(new File(filePath, s"$id.xml")) { fw =>
      fw.write(
        """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" +
        root.state.toHtml
      )
    }
  }
}
