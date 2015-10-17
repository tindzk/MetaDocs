package pl.metastack.metadocs.input.metadocs

import pl.metastack.metadocs.input._
import pl.metastack.metadocs.{Errata, document}

/** Converts input tree to document tree */
class Conversion(val instructionSet: InstructionSet,
                 val generateId: String => Option[String],
                 val errata: Errata = new Errata) {
  def convertRoot(root: tree.Root): document.tree.Root =
    document.tree.Root(root.children.map(convert): _*)

  def convertText(text: tree.Text): document.tree.Text =
    document.tree.Text(text.value)

  def convertTag(tag: tree.Tag): document.tree.Node =
    instructionSet.resolve(tag.name) match {
      case Some(instruction) => instruction.documentNode(this, tag)
        .asInstanceOf[document.tree.Node]
      case None =>
        errata.error(s"Tag '${tag.name}' not found in instruction set", tag)
        document.tree.Text("")
    }

  def convert(node: tree.Node): document.tree.Node =
    node match {
      case root: tree.Root => convertRoot(root)
      case text: tree.Text => convertText(text)
      case tag: tree.Tag => convertTag(tag)
    }

  /** All converted children */
  def childrenOf(tag: tree.Tag): Seq[document.tree.Node] =
    tag.children.map(convert)

  /** All children must have the given instruction type */
  def childrenOf
    [T <: document.tree.Node](tag: tree.Tag,
                              instruction: Instruction[T]): Seq[T] = {
    if (tag.children.exists {
      case text: tree.Text if text.text.trim.nonEmpty => true
      case _ => false
    }) errata.error(s"Text not allowed in tag's children", tag)

    tag.children.foreach {
      case child: tree.Tag
        if !instructionSet.resolve(child.name).contains(instruction) =>
        errata.error(s"Tag '${child.name}' is not a viable child of '${tag.name}'", child)

      case _ =>
    }

    tag.children.collect {
      case child: tree.Tag => child
    }.map(convert)
     .asInstanceOf[Seq[T]]
  }
}
