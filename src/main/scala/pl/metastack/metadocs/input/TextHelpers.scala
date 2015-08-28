package pl.metastack.metadocs.input

import scala.collection.mutable

import pl.metastack.metadocs.document

object TextHelpers {
  def reindent(text: String): String = {
    val lines = text.split("\n").dropWhile(_.trim.isEmpty)

    if (lines.isEmpty) ""
    else {
      val offsets = lines.map { line =>
        line.zipWithIndex.collectFirst {
          case (c, i) if !c.isWhitespace => i
        }
      }

      val leftMargin = offsets.collect { case Some(i) => i }.min

      val reindented = lines.map(_.drop(leftMargin))
      reindented.mkString("\n").trim
    }
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
