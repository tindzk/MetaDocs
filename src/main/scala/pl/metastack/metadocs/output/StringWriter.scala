package pl.metastack.metadocs.output

import scala.reflect.ClassTag

import pl.metastack.metadocs.document.tree

trait StringWriter[N <: tree.Node] extends Writer[N, String]

object StringWriter {
  def apply[N <: tree.Node](f: N => String)
                           (implicit ct: ClassTag[N]): StringWriter[N] =
    new StringWriter[N] {
      def write: PartialFunction[N, String] = {
        case node: N => f(node)
      }
    }

  def combine[N <: tree.Node](writer: StringWriter[N],
                              writers: StringWriter[N]*): StringWriter[N] =
    new StringWriter[N] {
      def write: PartialFunction[N, String] =
        writers.foldLeft(writer.write) { (acc, cur) =>
          acc.orElse(cur.write)
        }
    }
}
