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

  lazy val moves: List[Move] = situation.generateMoves.filter(_.orig == pos)

  lazy val destinations: List[Pos] = moves.map(_.dest)

  inline def color               = piece.color
  inline def is(inline c: Color) = c == piece.color
  inline def is(inline r: Role)  = r == piece.role
  inline def is(inline p: Piece) = p == piece

  def castleOn(side: Side): List[Move] = moves.filter(_.castle.exists(_.side == side))

object Actor:

  inline def pawnDirOf(inline color: Color): Direction = color.fold(_.up, _.down)

  /** Determines the position one ahead of a pawn based on the color of the piece.
    * White pawns move up and black pawns move down.
    */
  def posAheadOfPawn(pos: Pos, color: Color): Option[Pos] = pawnDirOf(color)(pos)

  /** Determines the squares that a pawn attacks based on the colour of the pawn.
    */
  def pawnAttacks(pos: Pos, color: Color): List[Pos] =
    color
      .fold(
        List(pos.upLeft, pos.upRight),
        List(pos.downLeft, pos.downRight)
      )
      .flatten
