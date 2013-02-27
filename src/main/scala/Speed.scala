package chess

sealed abstract class Speed(
  val id: Int, 
  val range: Range,
  val name: String) {
}

object Speed {

  case object Bullet extends Speed(1, 0 to 180, "Less than 3 minutes") 
  case object Blitz extends Speed(2, 181 to 480, "3 to 8 minutes") 
  case object Slow extends Speed(3, 481 to 1200, "8 to 20 minutes") 
  case object Unlimited extends Speed(4, 1200 to Int.MaxValue, "No clock") 

  val all = List(Bullet, Blitz, Slow, Unlimited)
  val byId = all map { v ⇒ (v.id, v) } toMap
  val byName = all map { v ⇒ (v.name, v) } toMap

  def apply(id: Int): Option[Speed] = byId get id

  def apply(clock: Option[Clock]) = byTime(clock.fold(Int.MaxValue)(_.estimateTotalTime))

  def byTime(seconds: Int): Speed = all.find(_.range contains seconds) | Unlimited

  def exists(id: Int): Boolean = byId contains id
}
