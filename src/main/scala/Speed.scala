package chess

sealed abstract class Speed(
    val id: Int,
    val range: Range,
    val shortName: String,
    val name: String) {

  lazy val key = toString.toLowerCase
}

object Speed {

  case object Bullet extends Speed(1, 0 to 179, "Bullet", "Less than 3 minutes")
  case object Blitz extends Speed(2, 180 to 479, "Blitz", "3 to 8 minutes")
  case object Classical extends Speed(3, 480 to 1199, "Classical", "8 to 20 minutes")
  case object Unlimited extends Speed(4, 1200 to Int.MaxValue, "Unlimited", "No clock")

  val all = List(Bullet, Blitz, Classical, Unlimited)
  val byId = all map { v => (v.id, v) } toMap
  val byName = all map { v => (v.name, v) } toMap

  def apply(id: Int): Option[Speed] = byId get id

  def apply(clock: Option[Clock]) = byTime(clock.fold(Int.MaxValue)(_.estimateTotalTime))

  def isUnlimited(clock: Clock) = byTime(clock.estimateTotalTime) == Unlimited

  def byTime(seconds: Int): Speed = all.find(_.range contains seconds) | Unlimited

  def exists(id: Int): Boolean = byId contains id
}
