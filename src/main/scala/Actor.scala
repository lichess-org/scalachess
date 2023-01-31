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
