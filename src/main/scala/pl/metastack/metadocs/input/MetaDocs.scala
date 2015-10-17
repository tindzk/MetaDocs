package pl.metastack.metadocs.input

import java.io.File

import pl.metastack.metadocs._
import pl.metastack.metadocs.input.tree.Root
import pl.metastack.metadocs.document.Document

import scala.util.Try

object MetaDocs {
  /**
   * @param generateId If the ID of a structural element (chapter, section etc.)
   *                   is missing, this function will be called with the caption.
   */
  def toDocumentTree(root: Root,
                     instructionSet: InstructionSet,
                     generateId: String => Option[String] = _ => None
                    ): document.tree.Root = {
    val conversion = new input.Conversion(instructionSet, generateId)
    conversion.convertRoot(root)
  }

  def loadFile(file: File,
               instructionSet: InstructionSet,
               generateId: String => Option[String] = _ => None
              ): Try[document.tree.Root] = {
    val contents = io.Source.fromFile(file).mkString
    val root = Parser.parse(contents)
    root.map(toDocumentTree(_, instructionSet, generateId))
  }
}
