package pl.metastack.metadocs.input

import java.io.File

import pl.metastack.metadocs.FileUtils

import scala.util.Try
import pl.metastack.metadocs.document.tree.Root

object Markdown {
  def loadFile(file: File,
               sourcePath: String,
               constants: Map[String, String] = Map.empty,
               generateId: String => Option[String] = _ => None
              ): Try[Root] =
    FileUtils.readFile(file) { contents =>
      val replaced = Helpers.replaceConstants(contents.mkString, constants)
      Try(markdown.Pegdown.parse(replaced, markdown.Conversion(generateId))
        .copy(sourcePath = Some(sourcePath)))
    }

  def loadFileWithExtensions(file: File,
                             sourcePath: String,
                             instructionSet: metadocs.InstructionSet,
                             constants: Map[String, String] = Map.empty,
                             generateId: String => Option[String] = _ => None
                            ): Try[Root] =
    FileUtils.readFile(file) { contents =>
      val replaced = Helpers.replaceConstants(contents.mkString, constants)
      Try(markdown.Pegdown.parseWithExtensions(
        replaced, instructionSet, markdown.Conversion(generateId)
      ).copy(sourcePath = Some(sourcePath)))
    }
}
