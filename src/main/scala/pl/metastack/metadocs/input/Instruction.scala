package pl.metastack.metadocs.input

import pl.metastack.metadocs.{document, input}

case class ArgumentParser(name: String, default: Boolean) {
  def getStringOpt(conversion: Conversion, tag: input.tree.Tag): Option[String] = {
    val value = tag.argumentValue(name)
    if (default) value.orElse(tag.defaultArgument(conversion.errata))
    else value
  }

  def getString(conversion: Conversion, tag: input.tree.Tag): String =
    getStringOpt(conversion, tag).getOrElse {
      conversion.errata.error(s"Argument '$name' was not set", tag)
      ""
    }

  def getIntOpt(conversion: Conversion, tag: input.tree.Tag): Option[Int] =
    getStringOpt(conversion, tag).map(_.toInt)

  def getInt(conversion: Conversion, tag: input.tree.Tag): Int =
    getIntOpt(conversion, tag).getOrElse {
      conversion.errata.error(s"Argument '$name' was not set", tag)
      0
    }

  def getBooleanOpt(conversion: Conversion, tag: input.tree.Tag): Option[Boolean] =
    getStringOpt(conversion, tag).map(_ == "yes")

  def getBoolean(conversion: Conversion, tag: input.tree.Tag): Boolean =
    getBooleanOpt(conversion, tag).getOrElse {
      conversion.errata.error(s"Argument '$name' was not set", tag)
      false
    }
}

trait Instruction[T <: document.tree.Node] {
  val name: String
  def documentNode(conversion: Conversion, tag: input.tree.Tag): T

  def argument(name: String, default: Boolean): ArgumentParser =
    ArgumentParser(name, default)
}

case object Url extends Instruction[document.tree.Url] {
  val href = argument("href", default = true)

  override val name = "url"

  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Url =
    document.tree.Url(
      href.getString(conversion, tag),
      conversion.childrenOf(tag): _*)
}

case object Chapter extends Instruction[document.tree.Chapter] {
  val id = argument("id", default = false)
  val title = argument("title", default = true)

  override val name = "chapter"

  // TODO Forbid chapter within chapter
  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Chapter = {
    val idValue = id.getStringOpt(conversion, tag)
    val titleValue = title.getString(conversion, tag)

    document.tree.Chapter(
      idValue.orElse(conversion.generateId(titleValue)),
      titleValue,
      conversion.detectParagraphs(conversion.childrenOf(tag)): _*)
  }
}

case object Section extends Instruction[document.tree.Section] {
  val id = argument("id", default = false)
  val title = argument("title", default = true)

  override val name = "section"

  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Section = {
    val idValue = id.getStringOpt(conversion, tag)
    val titleValue = title.getString(conversion, tag)

    document.tree.Section(
      idValue.orElse(conversion.generateId(titleValue)),
      titleValue,
      conversion.detectParagraphs(conversion.childrenOf(tag)): _*)
  }
}

case object Subsection extends Instruction[document.tree.Subsection] {
  val id = argument("id", default = false)
  val title = argument("title", default = true)

  override val name = "subsection"

  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Subsection = {
    val idValue = id.getStringOpt(conversion, tag)
    val titleValue = title.getString(conversion, tag)

    document.tree.Subsection(
      idValue.orElse(conversion.generateId(titleValue)),
      titleValue,
      conversion.detectParagraphs(conversion.childrenOf(tag)): _*)
  }
}

case object Abstract extends Instruction[document.tree.Abstract] {
  override val name = "abstract"

  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Abstract =
    document.tree.Abstract(conversion.childrenOf(tag): _*)
}

case object Bold extends Instruction[document.tree.Bold] {
  override val name = "bold"
  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Bold =
    document.tree.Bold(conversion.childrenOf(tag): _*)
}

case object Italic extends Instruction[document.tree.Italic] {
  override val name = "italic"
  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Italic =
    document.tree.Italic(conversion.childrenOf(tag): _*)
}

case object Code extends Instruction[document.tree.Code] {
  override val name = "code"
  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Code =
    document.tree.Code(conversion.childrenOf(tag): _*)
}

case object Image extends Instruction[document.tree.Image] {
  val href = argument("href", default = true)

  override val name = "image"
  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Image =
    document.tree.Image(href.getString(conversion, tag))
}

case object List extends Instruction[document.tree.List] {
  override val name = "list"
  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.List =
    document.tree.List(conversion.childrenOf(tag, ListItem): _*)
}

case object ListItem extends Instruction[document.tree.ListItem] {
  override val name = "listItem"
  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.ListItem =
    document.tree.ListItem(conversion.childrenOf(tag): _*)
}

case object Table extends Instruction[document.tree.Table] {
  override val name = "table"
  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Table = {
    val children = conversion.childrenOf(tag, Row)

    if (children.isEmpty) {
      conversion.errata.error("Table must have header", tag)
      document.tree.Table(document.tree.Row())
    } else {
      val headerRow = children.head
      val bodyRows = children.tail

      document.tree.Table(headerRow, bodyRows: _*)
    }
  }
}

case object Row extends Instruction[document.tree.Row] {
  override val name = "row"

  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Row =
    document.tree.Row(conversion.childrenOf(tag, Column): _*)
}

case object Column extends Instruction[document.tree.Column] {
  override val name = "column"

  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Column =
    document.tree.Column(conversion.childrenOf(tag): _*)
}

case object Scala extends Instruction[document.tree.Scala] {
  val id = argument("id", default = false)
  val project = argument("project", default = false)
  val global = argument("global", default = false)
  val printResult = argument("printResult", default = false)
  val hidden = argument("hidden", default = false)

  override val name = "scala"

  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Scala = {
    val globalValue = global.getBooleanOpt(conversion, tag).getOrElse(false)
    var printResultValue = printResult.getBooleanOpt(conversion, tag).getOrElse(false)

    if (globalValue && printResultValue) {
      conversion.errata.error(
        "The attributes 'global' and 'printResult' are incompatible", tag)
      printResultValue = false
    }

    document.tree.Scala(
      id.getStringOpt(conversion, tag).getOrElse(conversion.listingId()),
      project.getStringOpt(conversion, tag),
      globalValue,
      printResultValue,
      hidden.getBooleanOpt(conversion, tag).getOrElse(false),
      conversion.reindent(tag.text))
  }
}

case object Sbt extends Instruction[document.tree.Sbt] {
  val project = argument("project", default = false)
  val hidden = argument("hidden", default = false)

  override val name = "sbt"

  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Sbt =
    document.tree.Sbt(
      conversion.listingId(),
      project.getStringOpt(conversion, tag),
      hidden.getBooleanOpt(conversion, tag).getOrElse(false),
      conversion.reindent(tag.text))
}

case object Shell extends Instruction[document.tree.Shell] {
  override val name = "shell"

  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Shell =
    document.tree.Shell(
      conversion.listingId(),
      conversion.reindent(tag.text))
}

case object Todo extends Instruction[document.tree.Todo] {
  override val name = "todo"

  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Todo =
    document.tree.Todo(conversion.childrenOf(tag): _*)
}

case object Jump extends Instruction[document.tree.Jump] {
  val ref = argument("ref", default = true)

  override val name = "jump"

  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Jump = {
    val text = tag.text.trim

    document.tree.Jump(
      ref.getString(conversion, tag),
      if (text.isEmpty) None else Some(text)
    )
  }
}

case object Footnote extends Instruction[document.tree.Footnote] {
  override val name = "footnote"

  override def documentNode(conversion: Conversion,
                            tag: input.tree.Tag): document.tree.Footnote =
    document.tree.Footnote(None, conversion.childrenOf(tag): _*)
}

trait InstructionSet {
  val instructions: Set[Instruction[_]]
  val aliases: Map[String, Instruction[_]] = Map.empty

  private val that = this

  def withAliases(myAliases: Map[String, Instruction[_]]): InstructionSet =
    new InstructionSet {
      override val instructions = that.instructions
      override val aliases = that.aliases ++ myAliases
    }

  def inherit(other: InstructionSet): InstructionSet = new InstructionSet {
    override val instructions = that.instructions ++ other.instructions
    override val aliases = that.aliases ++ other.aliases
  }

  def resolve(name: String): Option[Instruction[_]] =
    instructions.find(_.name == name)
      .orElse(aliases.get(name))
}

object DefaultInstructionSet extends InstructionSet {
  override val instructions: Set[Instruction[_]] = Set(
    Abstract, Jump, Footnote, Chapter, Section, Subsection, Bold, Italic, Url,
    List, ListItem, Code, Image, Table, Row, Column)
}

object CodeInstructionSet extends InstructionSet {
  override val instructions: Set[Instruction[_]] = Set(Scala, Sbt, Shell)
}

object DraftInstructionSet extends InstructionSet {
  override val instructions: Set[Instruction[_]] = Set(Todo)
}