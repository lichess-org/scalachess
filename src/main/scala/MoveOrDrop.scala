package chess

import chess.format.Uci

type MoveOrDrop = Move | Drop

object MoveOrDrop:
  extension (md: MoveOrDrop)
    def isMove = md.isInstanceOf[Move]
    def isDrop = md.isInstanceOf[Drop]

    def fold[A](move: Move => A, drop: Drop => A): A =
      md match
        case m: Move => move(m)
        case d: Drop => drop(d)

    def move: Option[Move] = md.fold(Some(_), _ => None)
    def drop: Option[Drop] = md.fold(_ => None, Some(_))

    def applyVariantEffect: MoveOrDrop =
      md match
        case m: Move => m.applyVariantEffect
        case d: Drop => d

    def toUci: Uci =
      md match
        case m: Move => m.toUci
        case d: Drop => d.toUci
