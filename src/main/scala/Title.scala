package chess

import cats.Show

opaque type PlayerTitle = String

/* Not extending TotalWrapper
 * so that apply is private,
 * preventing creating exotic titles */
object PlayerTitle:

  private inline def apply(inline s: String): PlayerTitle = s.asInstanceOf[PlayerTitle]
  extension (t: PlayerTitle)
    inline def value: String  = t
    def isLichess: Boolean    = t == "LM" || t == "BOT"
    def isFederation: Boolean = !isLichess

  given Show[PlayerTitle] = Show.show(_.value)

  val GM: PlayerTitle  = "GM"
  val WGM: PlayerTitle = "WGM"
  val IM: PlayerTitle  = "IM"
  val WIM: PlayerTitle = "WIM"
  val FM: PlayerTitle  = "FM"
  val WFM: PlayerTitle = "WFM"
  val NM: PlayerTitle  = "NM"
  val CM: PlayerTitle  = "CM"
  val WCM: PlayerTitle = "WCM"
  val WNM: PlayerTitle = "WNM"
  val LM: PlayerTitle  = "LM"
  val BOT: PlayerTitle = "BOT"

  // names are as stated on FIDE profile pages
  val all = List[(PlayerTitle, String)](
    GM  -> "Grandmaster",
    WGM -> "Woman Grandmaster",
    IM  -> "International Master",
    WIM -> "Woman Intl. Master",
    FM  -> "FIDE Master",
    WFM -> "Woman FIDE Master",
    NM  -> "National Master",
    CM  -> "Candidate Master",
    WCM -> "Woman Candidate Master",
    WNM -> "Woman National Master",
    LM  -> "Lichess Master",
    BOT -> "Chess Robot"
  )

  val names: Map[PlayerTitle, String]          = all.toMap
  lazy val fromNames: Map[String, PlayerTitle] = all.map(_.swap).toMap

  val acronyms: List[PlayerTitle] = all.map(_._1)

  def titleName(title: PlayerTitle): String = names.getOrElse(title, title.value)

  def get(str: String): Option[PlayerTitle]      = Option(PlayerTitle(str.toUpperCase)).filter(names.contains)
  def get(strs: List[String]): List[PlayerTitle] = strs.flatMap { get(_) }

  // ordered by difficulty to achieve
  // if a player has multiple titles, the most valuable one is used
  private val titleRank: Map[PlayerTitle, Int] =
    List(GM, IM, WGM, FM, WIM, WFM, NM, CM, WCM, WNM).zipWithIndex.toMap

  def mostValuable(t1: Option[PlayerTitle], t2: Option[PlayerTitle]): Option[PlayerTitle] =
    t1.flatMap(titleRank.get)
      .fold(t2): v1 =>
        t2.flatMap(titleRank.get)
          .fold(t1): v2 =>
            if v1 < v2 then t1 else t2
