package chess

import cats.syntax.all.*
import chess.format.Uci
import chess.format.pgn.SanStr

import scala.annotation.threadUnsafe

sealed trait MoveOrDrop:

  inline def fold[A](move: Move => A, drop: Drop => A): A =
    this match
      case m: Move => move(m)
      case d: Drop => drop(d)

  def move: Option[Move] = this.fold(Some(_), _ => None)
  def drop: Option[Drop] = this.fold(_ => None, Some(_))

  def color: Color

  def before: Position

  def after: Position

  def toUci: Uci

  def toSanStr: SanStr

  inline def applyGame(game: Game): Game =
    this match
      case m: Move => game(m)
      case d: Drop => game.applyDrop(d)

object MoveOrDrop:

  case class WithPly(moveOrDrop: MoveOrDrop, ply: Ply):
    export moveOrDrop.*

case class Move(
    piece: Piece,
    orig: Square,
    dest: Square,
    before: Position,
    private[chess] val afterWithoutHistory: Position,
    capture: Option[Square],
    promotion: Option[PromotableRole],
    castle: Option[Move.Castle],
    enpassant: Boolean,
    metrics: MoveMetrics = MoveMetrics.empty
) extends MoveOrDrop:

  override def after: Position = finalizeHistory.copy()

  /* return whether this move captures an opponent piece */
  inline def captures: Boolean = capture.isDefined

  inline def promotes: Boolean = promotion.isDefined

  inline def castles: Boolean = castle.isDefined

  /* king to rook is the only variant-proof castling representation
   * it's required for chess960 where king and rook can start anywhere
   * and just works reqardless of variant
   */
  inline def normalizeCastle: Move =
    castle.fold(this): x =>
      copy(dest = x.rook)

  override inline def color: Color = piece.color

  inline def withMetrics(m: MoveMetrics): Move = copy(metrics = m)

  override lazy val toSanStr: SanStr = format.pgn.Dumper(this)
  override lazy val toUci: Uci.Move = Uci.Move(orig, dest, promotion)

  override def toString = s"$piece ${toUci.uci}"

  @threadUnsafe
  private lazy val finalizeHistory: Position =
    val after = this.afterWithoutHistory
      .withColor(!piece.color)
      .updateHistory { h =>
        h.copy(
          lastMove = Option(toUci),
          halfMoveClock =
            if piece.is(Pawn) || captures || promotes then HalfMoveClock.initial
            else h.halfMoveClock.incr,
          castlingRights = newCastlingRights
        )
      }

    // Update position hashes last, only after updating the board,
    // castling rights and en-passant rights.
    after.updateHistory { h =>
      lazy val positionHashesOfBoardBefore =
        if h.positionHashes.isEmpty then PositionHash(Hash(before)) else h.positionHashes
      val resetsPositionHashes = after.variant.isIrreversible(this)
      val basePositionHashes =
        if resetsPositionHashes then PositionHash.empty else positionHashesOfBoardBefore
      h.copy(positionHashes = PositionHash(Hash(after)).combine(basePositionHashes))
    }

  private def newCastlingRights: CastlingRights =
    var cr = afterWithoutHistory.history.castlingRights
    // A captured rook loses its castling right.
    if captures then cr = cr & ~dest.bl
    // A moving rook loses its castling right.
    if piece.is(Rook) then cr = cr & ~orig.bl
    // A moving king forfeits all of its color's castling rights.
    else if piece.is(King) then cr = cr.without(piece.color)
    cr

end Move

object Move:

  case class Castle(king: Square, kingTo: Square, rook: Square, rookTo: Square):
    def side: Side = if kingTo.file == File.C then QueenSide else KingSide
    def isStandard: Boolean = king.file == File.E && (rook.file == File.A || rook.file == File.H)

case class Drop(
    piece: Piece,
    square: Square,
    before: Position,
    private val afterWithoutHistory: Position,
    metrics: MoveMetrics = MoveMetrics.empty
) extends MoveOrDrop:

  override def after: Position = finalizeHistory

  inline def withMetrics(m: MoveMetrics): Drop = copy(metrics = m)

  override inline def color: Color = piece.color
  override lazy val toSanStr: SanStr = format.pgn.Dumper(this)
  override lazy val toUci: Uci.Drop = Uci.Drop(piece.role, square)

  override def toString = toUci.uci

  @threadUnsafe
  private lazy val finalizeHistory: Position =
    val after = this.afterWithoutHistory.withColor(!piece.color)
    after
      .updateHistory { h =>
        val basePositionHashes =
          if h.positionHashes.value.isEmpty then PositionHash(Hash(before)) else h.positionHashes
        h.copy(
          lastMove = toUci.some,
          castlingRights = before.castlingRights,
          halfMoveClock = if piece.is(Pawn) then HalfMoveClock.initial else h.halfMoveClock.incr,
          positionHashes = PositionHash(Hash(after)).combine(basePositionHashes)
        )
      }
