package pl.metastack.metadocs

class Errata() {
  def error(message: String, tag: Any): Unit = {
    println(s"[error] $message")
    println(s"  $tag")
  }
}
