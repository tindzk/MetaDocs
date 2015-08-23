package pl.metastack.metadocs.document.writer

import java.io.File

import scala.collection.mutable

import pl.metastack.metadocs.FileUtils
import pl.metastack.metadocs.document.tree

class SbtScala(projectsPath: String) {
  def Prologue(projectName: String) =
    """
    object Utils {
      def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
        val p = new java.io.PrintWriter(f)
        try op(p) finally p.close()
      }

      val results = new java.io.File("results")
      results.mkdir()

      def writeResult(listingId: String, result: Any) {
        val file = new java.io.File(results, listingId)
        printToFile(file)(_.write(result.toString))
      }
    }
    """

  case class Project(var sbtImports: String = "",
                     var scalaGlobal: String = "",
                     var scalaMain: String = "",
                     functionCalls: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty[String])

  def createProjects(root: tree.Root) {
    val output = new File(projectsPath)
    output.mkdirs()

    val projects = mutable.HashMap.empty[String, Project]

    def writeSbt(projectName: String, imports: String) {
      val path = new File(output, projectName)
      path.mkdirs()

      val file = new File(path, "build.sbt")
      FileUtils.printToFile(file) { writer =>
        writer.write(
          s"""
          name := "Auto-generated MetaDocs project"

          version := "0.1.0"

          scalaVersion := "2.11.7"

          $imports
          """)
      }
    }

    def iterate(node: tree.Node) {
      node match {
        case sbt @ tree.Sbt(id, project, hidden, code) =>
          project.foreach { p =>
            if (!projects.contains(p)) projects +=
              p -> Project(scalaGlobal = Prologue(p))
            projects(p).sbtImports += sbt.code + "\n"
          }

        case scala: tree.Scala =>
          scala.project.foreach { projectName =>
            val project = projects(projectName)

            if (scala.global) project.scalaGlobal += scala.code + "\n"
            else {
              if (scala.printResult) {
                project.scalaMain += s"def ${scala.id}() = {\n"
                project.functionCalls +=
                  s"""Utils.writeResult("${scala.id}.txt", ${scala.id}())"""
              } else {
                project.scalaMain += s"def ${scala.id}() {\n"
                project.functionCalls += s"${scala.id}()"
              }

              project.scalaMain += scala.code
              project.scalaMain += "}\n"
            }
          }

        case node: tree.Node => node.children.foreach(iterate)
      }
    }

    root.children.foreach(iterate)

    projects.foreach { case (projectName, project) =>
      writeSbt(projectName, project.sbtImports)

      val sourceDir = new File(projectsPath, s"$projectName/src/main/scala/")
      sourceDir.mkdirs()

      val mainFile = new File(sourceDir, "Main.scala")
      FileUtils.printToFile(mainFile) { fw =>
        fw.write(project.scalaGlobal)
        fw.write(
          s"""
          object Main {
            ${project.scalaMain}
            def main(args: Array[String]) {
              ${project.functionCalls.mkString("\n")}
            }
          }
          """)
      }
    }
  }

  def listingPath(scala: tree.Scala): String =
    s"$projectsPath/${scala.project.get}/results/${scala.id}.txt"

  /** Executes project only when output for at least one listing is missing */
  def runProjects(root: tree.Root, force: Boolean = false) {
    def mustExecute(node: tree.Node): Boolean = {
      node match {
        case scala: tree.Scala if scala.project.isDefined && scala.printResult =>
          val path = new File(listingPath(scala))

          if (path.exists()) false
          else {
            println(s"[info] Code listing '${scala.id}' does not exist yet")
            true
          }

        case node: tree.Node => node.children.exists(mustExecute)
      }
    }

    if (force || mustExecute(root)) {
      val projects = mutable.Set.empty[String]

      def populateProjects(node: tree.Node) {
        node match {
          case sbt: tree.Sbt if sbt.project.nonEmpty => projects += sbt.project.get
          case node: tree.Node => node.children.foreach(populateProjects)
        }
      }

      populateProjects(root)

      println(s"""[info] Running projects: ${projects.mkString(", ")}""")

      projects.foreach { project =>
        sys.process.Process(
          Seq("sbt", "run"),
          new java.io.File(projectsPath + "/" + project)).!!
      }
    }
  }

  def embedOutput(root: tree.Root): tree.Root = {
    root.map {
      case scala: tree.Scala if scala.project.isDefined && scala.printResult =>
        val path = new File(listingPath(scala))
        scala.copy(result = Some(io.Source.fromFile(path).mkString))

      case node: tree.Node => node
    }.asInstanceOf[tree.Root]
  }
}
