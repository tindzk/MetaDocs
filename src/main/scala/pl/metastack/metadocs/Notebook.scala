package pl.metastack.metadocs

import java.io.File

import scala.collection.mutable.ListBuffer

case class Session(blocks: ListBuffer[Block] = ListBuffer.empty) {
  def serialiseBlocks(): Map[String, BlockResult] =
    blocks.map { b =>
      val lines = FileUtils.readFile(new File(b.file))(_.getLines().toList)
      b.name ->
        BlockResult(
          TextHelpers.reindent(
            lines.slice(b.lineStart, b.lineEnd).mkString("\n")),
          if (b.outputLines.isEmpty) None
          else Some(b.outputLines.map(_.value).mkString("\n"))
        )
    }.toMap
}

case class Line(value: String, `type`: String)

case class Block(name: String,
                 file: String,
                 lineStart: Int,
                 var lineEnd: Int,
                 outputLines: ListBuffer[Line])

case class BlockResult(code: String, result: Option[String])

object Notebook {
  def block(name: String)(implicit session: Session,
                          file: sourcecode.File,
                          line: sourcecode.Line): Unit = {
    if (session.blocks.nonEmpty)
      session.blocks.last.lineEnd = line.value - 1
    session.blocks += Block(name, file.value, line.value, -1, ListBuffer.empty)
  }

  def println[T](str: T)(implicit session: Session, manifest: Manifest[T]): Unit = {
    if (session.blocks.isEmpty) throw new Exception("No blocks defined")
    session.blocks.last.outputLines += Line(str.toString, manifest.toString)
  }

  def end()(implicit session: Session, line: sourcecode.Line): Unit = {
    if (session.blocks.isEmpty) throw new Exception("No blocks defined")
    session.blocks.last.lineEnd = line.value - 1
  }
}
