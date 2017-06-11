package pl.metastack.metadocs

import scala.io
import java.io.File

import scala.io.BufferedSource

private [metadocs] object FileUtils {
  def stripTomlExtension(str: String): String =
    if (str.endsWith(".toml")) str.substring(0, str.lastIndexOf('.'))
    else str

  def readFile[T](file: File)(f: BufferedSource => T): T = {
    val source = io.Source.fromFile(file)
    try f(source) finally source.close()
  }

  def printToFile(file: java.io.File)(f: java.io.PrintWriter => Unit): Unit = {
    val p = new java.io.PrintWriter(file)
    try f(p) finally p.close()
  }
}
