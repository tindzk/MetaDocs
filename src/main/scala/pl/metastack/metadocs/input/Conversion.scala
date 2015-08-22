package pl.metastack.metadocs.input

import pl.metastack.metadocs.document

import scala.collection.mutable

/** Converts input tree to document tree */
class Conversion(val instructionSet: InstructionSet,
                 val generateId: String => Option[String],
                 val errata: Errata = new Errata) {
  private var listingCount = 0
  def listingId(): String = {
    listingCount += 1
    s"listing$listingCount"
  }

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

  def reindent(text: String): String = {
    val lines = text.split("\n").dropWhile(_.trim.isEmpty)

    val offsets = lines.map { line =>
      line.zipWithIndex.collectFirst {
        case (c, i) if !c.isWhitespace => i
      }
    }

    val leftMargin = offsets.collect { case Some(i) => i }.min

    val reindented = lines.map(_.drop(leftMargin))
    reindented.mkString("\n").trim
  }

  def detectParagraphs(nodes: Seq[document.tree.Node]): Seq[document.tree.Node] = {
    def cutParagraph(nodes: Seq[document.tree.Node]):
      (Seq[document.tree.Node], Seq[document.tree.Node]) = {

      val paragraph = mutable.ArrayBuffer.empty[document.tree.Node]
      val rest = mutable.ArrayBuffer.empty[document.tree.Node]
      var accumulatingRest = false

      nodes.foreach {
        case node if accumulatingRest => rest += node

        case node @ document.tree.Text(text) =>
          val paragraphs = text.split("\n\n")
            .filter(_.trim.nonEmpty)

          if (paragraphs.length <= 1) paragraph += node
          else {
            paragraph += document.tree.Text(paragraphs.head)
            rest += document.tree.Text(paragraphs.tail.mkString("\n\n"))
            accumulatingRest = true
          }

        case node if node.block =>
          rest += node
          accumulatingRest = true

        case node => paragraph += node
      }

      (paragraph, rest)
    }

    val result = mutable.ArrayBuffer.empty[document.tree.Node]

    var n = nodes
    while (n.nonEmpty) {
      val (parag, rest) = cutParagraph(n)
      n =
        if (parag.nonEmpty) {
          if (parag.exists {
            case node @ document.tree.Text(text) => text.trim.nonEmpty
            case _ => true
          }) {
            val trimmed = parag.zipWithIndex.map {
              case (node @ document.tree.Text(text), i)
                if i == 0 => document.tree.Text(text.dropWhile(_.isWhitespace))

              case (node @ document.tree.Text(text), i)
                if i == parag.length - 1 =>
                document.tree.Text(text.reverse.dropWhile(_.isWhitespace).reverse)

              case (node, i) => node
            }

            result += document.tree.Paragraph(trimmed: _*)
          }

          rest
        } else {
          result ++= rest.takeWhile(_.block)
          rest.dropWhile(_.block)
        }
    }

    result
  }
}
