package chess

import cats.syntax.all.*
import chess.format.Uci
import chess.variant.Crazyhouse

import bitboard.Bitboard
import bitboard.Bitboard.*

case class Situation(board: Board):
  export board.{
    history,
    isOccupied,
    kingOf,
    variant,
    legalMoves,
    enPassantSquare,
    ourKing,
    ourKings,
    theirKing,
    theirKings,
    us,
    them,
    checkers,
    generateMovesAt,
    genKingAt,
    genEnPassant,
    potentialEpSquare,
    genNonKing,
    genNonKingAndNonPawn,
    genPawn,
    genKnight,
    genBishop,
    genQueen,
    genRook,
    genUnsafeKing,
    genSafeKing,
    genCastling,
    color,
    playable,
    end,
    checkMate,
    staleMate,
    autoDraw,
    opponentHasInsufficientMaterial,
    threefoldRepetition,
    variantEnd,
    check,
    checkOf,
    status,
    winner
  }
  export color.white as isWhiteTurn

  lazy val moves: Map[Square, List[Move]] =
    legalMoves.groupBy(_.orig)

  lazy val playerCanCapture: Boolean = legalMoves.exists(_.captures)

  lazy val destinations: Map[Square, Bitboard] = legalMoves.groupMapReduce(_.orig)(_.dest.bb)(_ | _)

  def drops: Option[List[Square]] =
    variant match
      case v: Crazyhouse.type => v.possibleDrops(board)
      case _                  => None

  def checkSquare: Option[Square] = if check.yes then board.ourKing else None

  def move(from: Square, to: Square, promotion: Option[PromotableRole]): Either[ErrorStr, Move] =
    variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Either[ErrorStr, Move] =
    variant.move(this, uci.orig, uci.dest, uci.promotion)

  def drop(role: Role, square: Square): Either[ErrorStr, Drop] =
    variant.drop(this, role, square)

  inline def updateHistory(inline f: History => History) =
    copy(board = board.updateHistory(f))

  def withVariant(variant: chess.variant.Variant): Situation =
    copy(board = board.withVariant(variant))

  def unary_! : Situation = copy(board = board.copy(color = !color))

  // ========================bitboard===========================

object Situation:

  def apply(variant: chess.variant.Variant): Situation = Situation(Board.init(variant, White))

  case class AndFullMoveNumber(situation: Situation, fullMoveNumber: FullMoveNumber):
    def ply = fullMoveNumber.ply(situation.color)
