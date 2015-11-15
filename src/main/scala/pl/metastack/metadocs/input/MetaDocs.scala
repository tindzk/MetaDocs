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

  def loadFile(path: String,
               instructionSet: metadocs.InstructionSet,
               constants: Map[String, String] = Map.empty,
               generateId: String => Option[String] = _ => None
              ): Either[SyntaxError, document.tree.Root] = {
    val contents = io.Source.fromFile(new File(path)).mkString
    val replaced = Helpers.replaceConstants(contents, constants)
    val root = input.metadocs.Parser.parse(replaced)
    root.right.map(toDocumentTree(_, instructionSet, generateId)
      .copy(sourcePath = Some(path)))
  }
}
