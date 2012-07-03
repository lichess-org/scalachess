package chess.format

sealed abstract class Nag(val code: Int)

object Nag {
  case object Blunder extends Nag(4)
  case object Mistake extends Nag(2)
  case object Inaccuracy extends Nag(6)
}
