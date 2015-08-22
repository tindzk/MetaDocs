package pl.metastack.metadocs

object FileUtils {
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try op(p) finally p.close()
  }
}
