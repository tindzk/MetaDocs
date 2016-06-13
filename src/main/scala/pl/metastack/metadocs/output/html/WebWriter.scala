package pl.metastack.metadocs.output.html

import pl.metastack.metadocs.output
import pl.metastack.metadocs.document.tree

import pl.metastack.{metaweb => web}

import scala.reflect.ClassTag

trait WebWriter[N <: tree.Node] extends output.Writer[N, Seq[web.tree.Node]]

object WebWriter {
  def apply[N <: tree.Node](f: N => Seq[web.tree.Node])
                           (implicit ct: ClassTag[N]): WebWriter[N] =
    new WebWriter[N] {
      def write: PartialFunction[N, Seq[web.tree.Node]] = {
        case node: N => f(node)
      }
    }

  def combine[N <: tree.Node](writer: WebWriter[N],
                              writers: WebWriter[N]*): WebWriter[N] =
    new WebWriter[N] {
      def write: PartialFunction[N, Seq[web.tree.Node]] =
        writers.foldLeft(writer.write) { (acc, cur) =>
          acc.orElse(cur.write)
        }
    }
}
