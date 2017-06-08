package chess
package format.pgn

import org.joda.time.DateTimeZone
import org.joda.time.format.DateTimeFormat

case class Tag(name: TagType, value: String) {

  override def toString = s"""[$name "$value"]"""
}

sealed trait TagType {
  lazy val name = toString
  lazy val lowercase = name.toLowerCase
  val isUnknown = false
}

object Tag {

  case object Event extends TagType
  case object Site extends TagType
  case object Date extends TagType
  case object UTCDate extends TagType {
    val format = DateTimeFormat forPattern "yyyy.MM.dd" withZone DateTimeZone.UTC
  }
  case object UTCTime extends TagType {
    val format = DateTimeFormat forPattern "HH:mm:ss" withZone DateTimeZone.UTC
  }
  case object Round extends TagType
  case object White extends TagType
  case object Black extends TagType
  case object TimeControl extends TagType
  case object WhiteClock extends TagType
  case object BlackClock extends TagType
  case object WhiteElo extends TagType
  case object BlackElo extends TagType
  case object WhiteGlicko2 extends TagType
  case object BlackGlicko2 extends TagType
  case object WhiteTitle extends TagType
  case object BlackTitle extends TagType
  case object WhiteTeam extends TagType
  case object BlackTeam extends TagType
  case object Result extends TagType
  case object FEN extends TagType
  case object Variant extends TagType
  case object ECO extends TagType
  case object Opening extends TagType
  case object Termination extends TagType
  case object Annotator extends TagType
  case class Unknown(n: String) extends TagType {
    override def toString = n
    override val isUnknown = true
  }

  val tagTypes = List(
    Event, Site, Date, UTCDate, UTCTime, Round, White, Black, TimeControl,
    WhiteClock, BlackClock, WhiteElo, BlackElo, WhiteGlicko2, BlackGlicko2, WhiteTitle, BlackTitle,
    WhiteTeam, BlackTeam, Result, FEN, Variant, ECO, Opening, Termination, Annotator
  )
  val tagTypesByLowercase: Map[String, TagType] =
    tagTypes.map { t => t.lowercase -> t }(scala.collection.breakOut)

  def apply(name: String, value: Any): Tag = new Tag(
    name = tagType(name),
    value = value.toString
  )

  def apply(name: Tag.type => TagType, value: Any): Tag = new Tag(
    name = name(this),
    value = value.toString
  )

  def tagType(name: String) =
    (tagTypesByLowercase get name.toLowerCase) | Unknown(name)

  def find(tags: List[Tag], name: String): Option[String] =
    (Tag tagType name) |> { tagType =>
      tags.find(_.name == tagType).map(_.value)
    }
}
