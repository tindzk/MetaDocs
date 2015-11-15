package pl.metastack.metadocs.input

import java.io.File

import scala.util.Try

import pl.metastack.metadocs.document.tree.Root

object Markdown {
  def loadFile(path: String,
               constants: Map[String, String] = Map.empty,
               generateId: String => Option[String] = _ => None
              ): Try[Root] = {
    val contents = io.Source.fromFile(new File(path)).mkString
    val replaced = Helpers.replaceConstants(contents, constants)
    Try(markdown.Pegdown.parse(replaced, markdown.Conversion(generateId))
      .copy(sourcePath = Some(path)))
  }

  def loadFileWithExtensions(path: String,
                             instructionSet: metadocs.InstructionSet,
                             constants: Map[String, String] = Map.empty,
                             generateId: String => Option[String] = _ => None
                            ): Try[Root] = {
    val contents = io.Source.fromFile(new File(path)).mkString
    val replaced = Helpers.replaceConstants(contents, constants)
    Try(markdown.Pegdown.parseWithExtensions(
      replaced, instructionSet, markdown.Conversion(generateId)
    ).copy(sourcePath = Some(path)))
  }
}
