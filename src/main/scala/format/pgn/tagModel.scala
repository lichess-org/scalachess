package chess
package format.pgn

case class Tag(name: TagName, value: String) {

  override def toString = """[%s "%s"]""".format(name, value)
}

sealed trait TagName {
  lazy val name = toString
  lazy val lowercase = name.toLowerCase
}

object Tag {

  case object Event extends TagName
  case object Site extends Site
  case object Date extends Date
  case object White extends TagName
  case object Black extends TagName
  case object Result extends Date
  case object FEN extends TagName
  case object Variant extends TagName
  case object ECO extends TagName
  case class Unknown(n: String) extends TagName {
    override def toString = n
  }

  val knownTagNames = List(Event, Site, Date, White, Black, Result, FEN, Variant, ECO)

  def apply(name: String, value: Any): Tag = new Tag(
    name = tagName(name),
    value = value.toString)

  def apply(name: Tag.type ⇒ TagName, value: Any): Tag = new Tag(
    name = name(this),
    value = value.toString)

  def tagName(name: String) = {
    knownTagNames find (_.lowercase == name.toLowerCase)
  } | Unknown(name)
}
