package chess

import chess.format.Uci
import chess.format.pgn.SanStr
import chess.format.pgn.Dumper

type MoveOrDrop = Move | Drop

object MoveOrDrop:
  extension (md: MoveOrDrop)
    def isMove = md.isInstanceOf[Move]
    def isDrop = md.isInstanceOf[Drop]

    inline def fold[A](move: Move => A, drop: Drop => A): A =
      md match
        case m: Move => move(m)
        case d: Drop => drop(d)

    def move: Option[Move] = md.fold(Some(_), _ => None)
    def drop: Option[Drop] = md.fold(_ => None, Some(_))

    inline def applyVariantEffect: MoveOrDrop =
      md match
        case m: Move => m.applyVariantEffect
        case d: Drop => d

    inline def finalizeAfter: Board =
      md match
        case m: Move => m.finalizeAfter
        case d: Drop => d.finalizeAfter

    inline def situationBefore: Situation =
      md match
        case m: Move => m.situationBefore
        case d: Drop => d.situationBefore

    inline def situationAfter: Situation =
      md match
        case m: Move => m.situationAfter
        case d: Drop => d.situationAfter

    inline def toUci: Uci =
      md match
        case m: Move => m.toUci
        case d: Drop => d.toUci

    inline def toSanStr: SanStr =
      md match
        case m: Move => Dumper(m.situationBefore, m, m.situationAfter)
        case d: Drop => Dumper(d, d.situationAfter)

    inline def applyGame(game: Game): Game =
      md match
        case m: Move => game(m)
        case d: Drop => game.applyDrop(d)
