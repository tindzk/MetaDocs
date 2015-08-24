package pl.metastack.metadocs.document.writer.html

import pl.metastack.metadocs.document.tree
import pl.metastack.metadocs.document.writer

import pl.metastack.{metaweb => web}

import scala.reflect.ClassTag

trait WebWriter[N <: tree.Node] extends writer.Writer[N, web.tree.Node]

object WebWriter {
  def apply[N <: tree.Node](f: N => web.tree.Node)
                           (implicit ct: ClassTag[N]): WebWriter[N] =
    new WebWriter[N] {
      def write: PartialFunction[N, web.tree.Node] = {
        case node: N => f(node)
      }
    }

  def combine[N <: tree.Node](writer: WebWriter[N],
                              writers: WebWriter[N]*): WebWriter[N] =
    new WebWriter[N] {
      def write: PartialFunction[N, web.tree.Node] =
        writers.foldLeft(writer.write) { (acc, cur) =>
          acc.orElse(cur.write)
        }
    }
}
