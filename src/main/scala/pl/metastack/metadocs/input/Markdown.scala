package pl.metastack.metadocs.input

import java.io.File

import scala.util.Try

import pl.metastack.metadocs.input
import pl.metastack.metadocs.document.Document
import pl.metastack.metadocs.document.tree.Root

object Markdown {
  def loadFile(file: File,
               generateId: String => Option[String] = _ => None): Try[Root] = {
    val contents = io.Source.fromFile(file).mkString
    Try(markdown.Pegdown.parse(contents, markdown.Conversion(generateId)))
  }

  def loadFileWithExtensions(file: File,
                             instructionSet: input.InstructionSet,
                             generateId: String => Option[String] = _ => None): Try[Root] = {
    val contents = io.Source.fromFile(file).mkString
    Try(
      markdown.Pegdown.parseWithExtensions(
        contents, instructionSet, markdown.Conversion(generateId)))
  }
}
