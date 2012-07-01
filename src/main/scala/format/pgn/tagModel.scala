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

  case object FEN extends TagName
  case object Variant extends TagName
  case object White extends TagName
  case object Black extends TagName
  case object ECO extends TagName
  case class Unknown(n: String) extends TagName {
    override def toString = n
  }

  val knownTagNames = List(FEN, Variant, White, Black, ECO)

  def apply(name: String, value: String): Tag = new Tag(
    name = tagName(name),
    value = value)

  def apply(name: Tag.type => TagName, value: String): Tag = new Tag(
    name = name(this),
    value = value)

  def tagName(name: String) = {
    knownTagNames find (_.lowercase == name.toLowerCase)
  } | Unknown(name)
}
