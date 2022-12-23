package chess
package bitboard

import scala.collection.mutable.ListBuffer

import org.lichess.compression.game.{ Board as CBoard, Move as CMove, MoveList, Role as CRole } // scalafix:ok

object Helpers:

  import scala.language.implicitConversions

  import Bitboard.*
  given Conversion[Bitboard, Long] = _.value
  given Conversion[Long, Bitboard] = _.bb

  extension (cb: CBoard)
    def fen(halfMoves: Int, fullMoves: Int) =
      val b = Board(
        pawns = cb.pawns,
        knights = cb.knights,
        bishops = cb.bishops,
        rooks = cb.rooks,
        queens = cb.queens,
        kings = cb.kings,
        white = cb.white,
        black = cb.black,
        occupied = cb.occupied
      )
      val epSquare = if cb.epSquare == 0 then None else Pos.at(cb.epSquare)
      val state = State(
        turn = Color.fromWhite(cb.turn),
        epSquare = epSquare,
        castlingRights = cb.castlingRights,
        halfMoves = halfMoves,
        fullMoves = fullMoves
      )
      Fen(b, state)

  extension (f: Fen)
    def cBoard: CBoard =
      CBoard(
        f.board.pawns,
        f.board.knights,
        f.board.bishops,
        f.board.rooks,
        f.board.queens,
        f.board.kings,
        f.board.white,
        f.board.black,
        f.isWhiteTurn,
        f.state.epSquare.fold(0)(p => p.value),
        f.state.castlingRights
      )

  extension (ml: MoveList)
    def uciSet: Set[String] =
      val buffer = ListBuffer[String]()
      (0 until ml.size).foreach(i => buffer.addOne(ml.get(i).uci))
      buffer.toSet

  extension (r: Role)
    def cRole: CRole =
      r match
        case Pawn   => CRole.PAWN
        case Knight => CRole.KNIGHT
        case Bishop => CRole.BISHOP
        case Rook   => CRole.ROOK
        case Queen  => CRole.QUEEN
        case King   => CRole.KING

  extension (m: Move)
    def cMove: CMove =
      val cm = CMove()
      cm.from = m.from.value
      cm.to = m.to.value
      cm.capture = m.isCapture
      cm.role = m.role.cRole
      m match
        case Move.Normal(_, _, _, _) =>
          cm.`type` = CMove.NORMAL
        case Move.Promotion(_, _, p, _) =>
          cm.`type` = CMove.NORMAL
          cm.promotion = p.cRole
        case Move.EnPassant(_, _) =>
          cm.`type` = CMove.EN_PASSANT
        case Move.Castle(_, _) =>
          cm.`type` = CMove.CASTLING
      cm
