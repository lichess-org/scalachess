package chess

import format.Uci
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class Actor(
    piece: Piece,
    pos: Pos,
    board: Board
):

  import Actor.*

  lazy val situation = Situation(board, piece.color)

  lazy val moves: List[Move] = situation.generateMovesAt(pos)

  inline def color               = piece.color
  inline def is(inline c: Color) = c == piece.color
  inline def is(inline r: Role)  = r == piece.role
  inline def is(inline p: Piece) = p == piece

object Actor:

  import bitboard.Bitboard.{ occupiedSquares, pawnAttacks }
  inline def pawnDirOf(inline color: Color): Direction = color.fold(_.up, _.down)

  /** Determines the position one ahead of a pawn based on the color of the piece.
    * White pawns move up and black pawns move down.
    */
  def posAheadOfPawn(pos: Pos, color: Color): Option[Pos] = pawnDirOf(color)(pos)

  /** Determines the squares that a pawn attacks based on the colour of the pawn.
    */
  def pawnAttacks(pos: Pos, color: Color): List[Pos] =
    pos.pawnAttacks(color).occupiedSquares
