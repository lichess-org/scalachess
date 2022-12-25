package chess

import cats.data.Validated
import cats.implicits.*
import bitboard.Bitboard

import chess.format.Uci

case class Situation(board: Board, color: Color):

  lazy val actors = board actorsOf color

  // TODO this is the one
  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val playerCanCapture: Boolean = moves exists (_._2 exists (_.captures))

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _.map(_.dest) }.to(Map)

  def drops: Option[List[Pos]] =
    board.variant match
      case v: variant.Crazyhouse.type => v possibleDrops this
      case _                          => None

  lazy val kingPos: Option[Pos] = board kingPosOf color

  lazy val check: Boolean = board check color

  def checkSquare = if (check) kingPos else None

  inline def history = board.history

  inline def checkMate: Boolean = board.variant checkmate this

  inline def staleMate: Boolean = board.variant staleMate this

  inline def autoDraw: Boolean = board.autoDraw || board.variant.specialDraw(this)

  inline def opponentHasInsufficientMaterial: Boolean = board.variant.opponentHasInsufficientMaterial(this)

  lazy val threefoldRepetition: Boolean = board.history.threefoldRepetition

  inline def variantEnd = board.variant specialEnd this

  inline def end: Boolean = checkMate || staleMate || autoDraw || variantEnd

  inline def winner: Option[Color] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end && !copy(color = !color).check

  lazy val status: Option[Status] =
    if (checkMate) Status.Mate.some
    else if (variantEnd) Status.VariantEnd.some
    else if (staleMate) Status.Stalemate.some
    else if (autoDraw) Status.Draw.some
    else none

  def move(from: Pos, to: Pos, promotion: Option[PromotableRole]): Validated[String, Move] =
    board.variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest, uci.promotion)

  def drop(role: Role, pos: Pos): Validated[String, Drop] =
    board.variant.drop(this, role, pos)

  def withHistory(history: History) =
    copy(
      board = board withHistory history
    )

  def withVariant(variant: chess.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  export board.history.canCastle

  def enPassantSquareForHash: Option[Pos] =
    // Before potentially expensive move generation, first ensure some basic
    // conditions are met.
    // todo we can do better with bitboard
    history.lastMove match
      case Some(move: Uci.Move) =>
        if (
          move.dest.yDist(move.orig) == 2 &&
          board(move.dest).exists(_.is(Pawn)) &&
          List(
            move.dest.file.offset(-1),
            move.dest.file.offset(1)
          ).flatten.flatMap(board(_, color.passablePawnRank)).exists(_ == color.pawn)
        )
          moves.values.flatten.find(_.enpassant).map(_.dest)
        else None
      case _ => None

  def unary_! = copy(color = !color)

  // =======================================bitboard========================

  val ourKing                    = board.board.king(color)
  val us: Bitboard               = board.board.byColor(color)
  def them: Bitboard             = board.board.byColor(!color)
  def checkers: Option[Bitboard] = ourKing.map(board.board.attacksTo(_, !color))
  def sliderBlockers: Bitboard   = board.board.sliderBlockers(color)
  def isWhiteTurn: Boolean       = color.white
  def isOccupied: Pos => Boolean = board.board.isOccupied

  def isSafe(king: Pos, move: Move, blockers: Bitboard): Boolean = ???

object Situation:

  def apply(variant: chess.variant.Variant): Situation = Situation(Board init variant, White)

  case class AndFullMoveNumber(situation: Situation, fullMoveNumber: FullMoveNumber):
    def ply = fullMoveNumber.ply(situation.color)

  import bitboard.Bitboard.*

  import scala.collection.mutable.ListBuffer

  extension (f: Situation)
    def generate: List[Move] = ???
    // val king = s.ourKing

    def genEnPassant(ep: Pos): List[Move] =
      val pawns                                   = f.us & f.board.board.pawns & ep.pawnAttacks(!f.color)
      val ff: Bitboard => Option[(Pos, Bitboard)] = bb => bb.lsb.map((_, bb & (bb - 1L)))
      List.unfold(pawns)(ff).map(enpassant(_, ep))

    /** Generate all pawn moves except en passant
     *  This includes
      *   - captures
      *   - single square moves
      *   - double square moves
      * @mask:
      *   bitboard contains empty square or enemy pieces
      *
      *   TODO @mask includes enemy King now, which should not be because
      *   enemy King cannot be captured by law
      */
    def genPawn(mask: Bitboard): List[Move] =
      val moves = ListBuffer[Move]()

      // pawn captures
      val capturers = f.us & f.board.board.pawns

      val s1: List[List[Move]] = for
        from <- capturers.occupiedSquares
        targets = from.pawnAttacks(f.color) & f.them & mask
        to <- targets.occupiedSquares
      yield genPawnMoves(from, to, true)

      // normal pawn moves
      val singleMoves = ~f.board.occupied & (if f.isWhiteTurn then ((f.board.white & f.board.pawns) << 8)
                                             else ((f.board.black & f.board.pawns) >>> 8)) & mask

      val doubleMoves =
        ~f.board.occupied & (if f.isWhiteTurn then (singleMoves << 8) else (singleMoves >>> 8))
          & Bitboard.RANKS(if f.isWhiteTurn then 3 else 4) & mask

      val s2: List[List[Move]] = for
        to <- singleMoves.occupiedSquares
        from = Pos.at(to.value + (if f.isWhiteTurn then -8 else 8)).get
      yield genPawnMoves(from, to, false)

      val s3: List[Move] = for
        to <- doubleMoves.occupiedSquares
        from = Pos.at(to.value + (if f.isWhiteTurn then -16 else 16)).get
      yield normalMove(from, to, Pawn, false)

      s1.flatten ++ s2.flatten ++ s3

    private def genPawnMoves(from: Pos, to: Pos, capture: Boolean): List[Move] =
      if from.rank == f.color.seventhRank then
        List(Queen, Knight, Rook, Bishop).map(promotion(from, to, _, capture))
      else List(normalMove(from, to, Pawn, capture))

    private def enpassant(orig: Pos, dest: Pos) =
      val capture = Option(dest.combine(orig))
      val after   = f.board.taking(orig, dest, capture).get // todo we know that we have value
      Move(
        piece = f.color.pawn,
        orig = orig,
        dest = dest,
        situationBefore = f,
        after = after,
        capture = capture,
        castle = None,
        promotion = None,
        enpassant = true
      )

    private def normalMove(orig: Pos, dest: Pos, role: Role, capture: Boolean) =
      val taken = if capture then Option(dest) else None
      val after = if(capture) then f.board.taking(orig, dest, taken).get // todo no get pls
                  else f.board.move(orig, dest).get
      Move(
        piece = Piece(f.color, role),
        orig = orig,
        dest = dest,
        situationBefore = f,
        after = after,
        capture = taken,
        castle = None,
        promotion = None,
        enpassant = false
      )

    private def promotion(orig: Pos, dest: Pos, promotion: PromotableRole, capture: Boolean) =
      val taken = if capture then Option(dest) else None
      val after = if(capture) then f.board.taking(orig, dest, taken).get // todo no get pls
                  else f.board.move(orig, dest).get
      Move(
        piece = f.color.pawn,
        orig = orig,
        dest = dest,
        situationBefore = f,
        after = after,
        capture = taken,
        castle = None,
        promotion = Some(promotion),
        enpassant = false
      )
