package pl.metastack.metadocs.input

import java.io.File

import scala.util.Try

import pl.metastack.metadocs.document.tree.Root

object Markdown {
  def loadFile(file: File,
               constants: Map[String, String] = Map.empty,
               generateId: String => Option[String] = _ => None
              ): Try[Root] = {
    val contents = io.Source.fromFile(file).mkString
    val replaced = Helpers.replaceConstants(contents, constants)
    Try(markdown.Pegdown.parse(replaced, markdown.Conversion(generateId)))
  }

  def loadFileWithExtensions(file: File,
                             instructionSet: metadocs.InstructionSet,
                             constants: Map[String, String] = Map.empty,
                             generateId: String => Option[String] = _ => None
                            ): Try[Root] = {
    val contents = io.Source.fromFile(file).mkString
    val replaced = Helpers.replaceConstants(contents, constants)
    Try(
      markdown.Pegdown.parseWithExtensions(
        replaced, instructionSet, markdown.Conversion(generateId)))
  }
}
