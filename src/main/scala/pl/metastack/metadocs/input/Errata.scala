package pl.metastack.metadocs.input

class Errata() {
  def error(message: String, tag: tree.Tag) {
    println(s"[error] $message")
    println(s"  $tag")
  }
}
