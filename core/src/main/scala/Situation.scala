package chess

import cats.syntax.all.*
import chess.format.Uci
import chess.variant.Crazyhouse

import bitboard.Bitboard
import bitboard.Bitboard.*

case class Situation(board: Board, color: Color):
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
    genCastling
  }
  export color.white as isWhiteTurn

  lazy val moves: Map[Square, List[Move]] =
    legalMoves.groupBy(_.orig)

  lazy val playerCanCapture: Boolean = legalMoves.exists(_.captures)

  lazy val destinations: Map[Square, Bitboard] = legalMoves.groupMapReduce(_.orig)(_.dest.bb)(_ | _)

  def drops: Option[List[Square]] =
    variant match
      case v: Crazyhouse.type => v.possibleDrops(this)
      case _                  => None

  lazy val check: Check               = checkOf(color)
  inline def checkOf(c: Color): Check = variant.kingThreatened(board.board, c)

  def checkSquare: Option[Square] = if check.yes then board.ourKing else None

  inline def checkMate: Boolean = variant.checkmate(this)

  inline def staleMate: Boolean = variant.staleMate(this)

  inline def autoDraw: Boolean = variant.autoDraw(board) || variant.specialDraw(this)

  inline def opponentHasInsufficientMaterial: Boolean = variant.opponentHasInsufficientMaterial(this)

  lazy val threefoldRepetition: Boolean = history.threefoldRepetition

  inline def variantEnd: Boolean = variant.specialEnd(this)

  inline def end: Boolean = checkMate || staleMate || autoDraw || variantEnd

  inline def winner: Option[Color] = variant.winner(this)

  def playable(strict: Boolean): Boolean =
    board.variant.valid(this, strict) && !end && copy(color = !color).check.no

  lazy val status: Option[Status] =
    if checkMate then Status.Mate.some
    else if variantEnd then Status.VariantEnd.some
    else if staleMate then Status.Stalemate.some
    else if autoDraw then Status.Draw.some
    else none

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

  def unary_! : Situation = copy(color = !color)

  // ========================bitboard===========================

object Situation:

  def apply(variant: chess.variant.Variant): Situation = Situation(Board.init(variant), White)

  case class AndFullMoveNumber(situation: Situation, fullMoveNumber: FullMoveNumber):
    def ply = fullMoveNumber.ply(situation.color)
