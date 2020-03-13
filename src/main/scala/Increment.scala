package chess

sealed abstract class Increment(val id: Int) {

  lazy val name = toString.toLowerCase

  def yes = this == Increment.Yes
  def no  = this == Increment.No

  def fold[A](c: => A, r: => A): A = if (this.yes) c else r
}

object Increment {

  case object Yes extends Increment(0)
  case object No  extends Increment(1)

  val all = List(Yes, No)

  val byId = all map { v =>
    (v.id, v)
  } toMap

  def apply(id: Int): Option[Increment] = byId get id

  def apply(increment: Boolean) = increment.fold(Yes, No)

  val default: Increment = Yes

  def orDefault(id: Int): Increment = apply(id) | default
}