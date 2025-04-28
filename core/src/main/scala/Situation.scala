package chess

type Situation = Board

object Situation:

  def apply(variant: chess.variant.Variant): Situation = Board(variant)

  case class AndFullMoveNumber(situation: Situation, fullMoveNumber: FullMoveNumber):
    def ply = fullMoveNumber.ply(situation.color)
