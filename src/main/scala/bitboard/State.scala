package chess
package bitboard

import cats.syntax.all.*

import Bitboard.*

/** All the information needed to restart the game beside board position. We only deal with standard variant now, but in
  * the future each variant can and should have different state.
  */
// similar to chess.History
case class State(
    // color
    turn: Color,
    // possible en-passant square
    epSquare: Option[Pos],
    // 1 in position of a rook means castling right
    // isomorphic to chess.Castling case class
    // TODO more sophiciated type for Castle
    castlingRights: Bitboard,
    // The halfmove clock specifies a decimal number of half moves with respect
    // to the 50/75 move draw rule. It is reset to zero after a capture or a pawn
    // move and incremented otherwise.
    halfMoves: HalfMoveClock,
    // The number of the full moves in a game. It starts at 1,
    // and is incremented after each Black's move.
    fullMoves: FullMoveNumber
    // TODO add
    // lastMove: Option[Uci]
    // positionhash: positionhash
    // checkcount: Option[CheckCount]
    // unmovedRooks?????
):
  import Castles.*
  lazy val castles: chess.Castles =
    chess.Castles(
      castlingRights.whiteKingSide,
      castlingRights.whiteQueenSide,
      castlingRights.blackKingSide,
      castlingRights.blackQueenSide
    )

object State:
  val start = State(Color.White, None, Bitboard.corners, HalfMoveClock(0), FullMoveNumber(1))

type Castles = Bitboard
object Castles:
  def fromChessCastle(c: chess.Castles): Castles =
    val whiteKing  = if (c.whiteKingSide) Pos.H1.bitboard else Bitboard.empty
    val whiteQueen = if (c.whiteQueenSide) Pos.H1.bitboard else Bitboard.empty
    val blackKing  = if (c.blackKingSide) Pos.H1.bitboard else Bitboard.empty
    val blackQueen = if (c.blackQueenSide) Pos.H1.bitboard else Bitboard.empty
    whiteKing & whiteQueen & blackKing & blackQueen

  extension (c: Castles)
    def whiteKingSide  = (c & Pos.H1.bitboard).isNotEmpty
    def whiteQueenSide = (c & Pos.A1.bitboard).isNotEmpty
    def blackKingSide  = (c & Pos.H8.bitboard).isNotEmpty
    def blackQueenSide = (c & Pos.A8.bitboard).isNotEmpty