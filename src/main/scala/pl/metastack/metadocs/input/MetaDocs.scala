package pl.metastack.metadocs.input

import java.io.File

import scala.util.Try

import pl.metastack.metadocs._

object MetaDocs {
  /**
   * @param generateId If the ID of a structural element (chapter, section etc.)
   *                   is missing, this function will be called with the caption.
   */
  def toDocumentTree(root: metadocs.tree.Root,
                     instructionSet: metadocs.InstructionSet,
                     generateId: String => Option[String] = _ => None
                    ): document.tree.Root = {
    val conversion = new metadocs.Conversion(instructionSet, generateId)
    conversion.convertRoot(root)
  }

  def loadFile(file: File,
               instructionSet: metadocs.InstructionSet,
               generateId: String => Option[String] = _ => None
              ): Try[document.tree.Root] = {
    val contents = io.Source.fromFile(file).mkString
    val root = input.metadocs.Parser.parse(contents)
    root.map(toDocumentTree(_, instructionSet, generateId))
  }
}