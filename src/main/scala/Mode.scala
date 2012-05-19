package chess

sealed abstract class Mode(val id: Int) {

  lazy val name = toString.toLowerCase

  def casual = this == Variant.Casual
  def rated = this == Variant.Rated

  def fold[A](c: ⇒ A, r: ⇒ A): A = if (this.casual) c else r
}

object Mode {

  case object Casual extends Mode(0)
  case object Rated extends Mode(1)

  val all = List(Casual, Rated)

  val byId = all map { v ⇒ (v.id, v) } toMap

  def apply(id: Int): Option[Mode] = byId get id

  val default = Casual

  def orDefault(id: Int): Mode = apply(id) | default
}
