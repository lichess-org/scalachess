package chess.format

import chess.Color

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def halfMove: Option[Int] = value.split(' ').lift(4).flatMap(_.toIntOption)
  def fullMove: Option[Int] = value.split(' ').lift(5).flatMap(_.toIntOption)

  def color: Option[Color] =
    value.split(' ').lift(1) flatMap (_.headOption) flatMap Color.apply

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (color.exists(_.white)) 2 else 1)
    }

  def initial = value == Forsyth.initial.value
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
