package chess

enum Speed(
    val id: Int,
    val key: String,
    val range: Range,
    val name: String,
    val title: String
):

  inline infix def >=(inline s: Speed): Boolean = range.min >= s.range.min
  inline infix def >(inline s: Speed): Boolean  = range.min > s.range.min
  inline infix def <=(inline s: Speed): Boolean = range.min <= s.range.min
  inline infix def <(inline s: Speed): Boolean  = range.min < s.range.min

  case UltraBullet
      extends Speed(0, "ultraBullet", 0 to 29, "UltraBullet", "Insanely fast games: less than 30 seconds")
  case Bullet extends Speed(1, "bullet", 30 to 179, "Bullet", "Very fast games: less than 3 minutes")
  case Blitz  extends Speed(2, "blitz", 180 to 479, "Blitz", "Fast games: 3 to 8 minutes")
  case Rapid  extends Speed(5, "rapid", 480 to 1499, "Rapid", "Rapid games: 8 to 25 minutes")
  case Classical
      extends Speed(3, "classical", 1500 to 21599, "Classical", "Classical games: 25 minutes and more")
  case Correspondence
      extends Speed(
        4,
        "correspondence",
        21600 to Int.MaxValue,
        "Correspondence",
        "Correspondence games: one or several days per move"
      )

object Speed:

  val all = values.toList

  given Ordering[Speed] = Ordering.by(_.range.min)

  val limited = List[Speed](Bullet, Blitz, Rapid, Classical)

  val byId = all.mapBy(_.id)

  export byId.{ contains as exists, get as apply }

  def apply(clock: Clock.Config) = byTime(clock.estimateTotalSeconds)

  def apply(clock: Option[Clock.Config]) = byTime(clock.fold(Int.MaxValue)(_.estimateTotalSeconds))

  def byTime(seconds: Int): Speed = all.find(_.range contains seconds) | Correspondence
