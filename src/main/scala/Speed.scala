package chess

opaque type SpeedId = Int
object SpeedId extends OpaqueInt[SpeedId]

opaque type SpeedKey = String
object SpeedKey extends OpaqueString[SpeedKey]

sealed class Speed(
    val id: SpeedId,
    val key: SpeedKey,
    val range: Range,
    val name: String,
    val title: String
) extends Ordered[Speed]:

  def compare(that: Speed) = range.min compare that.range.min

object Speed:

  case object UltraBullet
      extends Speed(
        SpeedId(0),
        SpeedKey("ultraBullet"),
        0 to 29,
        "UltraBullet",
        "Insanely fast games: less than 30 seconds"
      )
  case object Bullet
      extends Speed(
        SpeedId(1),
        SpeedKey("bullet"),
        30 to 179,
        "Bullet",
        "Very fast games: less than 3 minutes"
      )
  case object Blitz
      extends Speed(SpeedId(2), SpeedKey("blitz"), 180 to 479, "Blitz", "Fast games: 3 to 8 minutes")
  case object Rapid
      extends Speed(SpeedId(5), SpeedKey("rapid"), 480 to 1499, "Rapid", "Rapid games: 8 to 25 minutes")
  case object Classical
      extends Speed(
        SpeedId(3),
        SpeedKey("classical"),
        1500 to 21599,
        "Classical",
        "Classical games: 25 minutes and more"
      )
  case object Correspondence
      extends Speed(
        SpeedId(4),
        SpeedKey("correspondence"),
        21600 to Int.MaxValue,
        "Correspondence",
        "Correspondence games: one or several days per move"
      )

  val all = List(UltraBullet, Bullet, Blitz, Rapid, Classical, Correspondence)

  given Ordering[Speed] = Ordering.by(_.range.min)

  val limited = List[Speed](Bullet, Blitz, Rapid, Classical)

  val byId = all.mapBy(_.id)

  export byId.{ contains as exists, get as apply }

  def apply(clock: Clock.Config) = byTime(clock.estimateTotalSeconds)

  def apply(clock: Option[Clock.Config]) = byTime(clock.fold(Int.MaxValue)(_.estimateTotalSeconds))

  def byTime(seconds: Int): Speed = all.find(_.range contains seconds) | Correspondence
