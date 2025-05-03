package chess

import cats.syntax.all.*
import chess.format.Uci
import chess.format.pgn.SanStr

sealed trait MoveOrDrop:

  inline def fold[A](move: Move => A, drop: Drop => A): A =
    this match
      case m: Move => move(m)
      case d: Drop => drop(d)

  def move: Option[Move] = this.fold(Some(_), _ => None)
  def drop: Option[Drop] = this.fold(_ => None, Some(_))

  def color: Color

  def finalizeAfter: Position

  def boardBefore: Position

  def toUci: Uci

  def toSanStr: SanStr

  inline def applyGame(game: Game): Game =
    this match
      case m: Move => game(m)
      case d: Drop => game.applyDrop(d)

case class Move(
    piece: Piece,
    orig: Square,
    dest: Square,
    boardBefore: Position,
    after: Position,
    capture: Option[Square],
    promotion: Option[PromotableRole],
    castle: Option[Move.Castle],
    enpassant: Boolean,
    metrics: MoveMetrics = MoveMetrics.empty
) extends MoveOrDrop:

  override lazy val finalizeAfter: Position =
    val after = this.after
      .withColor(!piece.color)
      .updateHistory { h =>
        val (castles, unmovedRooks) = castleRights
        h.copy(
          lastMove = Option(toUci),
          halfMoveClock =
            if piece.is(Pawn) || captures || promotes then HalfMoveClock.initial
            else h.halfMoveClock.incr,
          castles = castles,
          unmovedRooks = unmovedRooks
        )
      }

    // Update position hashes last, only after updating the board,
    // castling rights and en-passant rights.
    after.updateHistory { h =>
      lazy val positionHashesOfBoardBefore =
        if h.positionHashes.isEmpty then PositionHash(Hash(boardBefore)) else h.positionHashes
      val resetsPositionHashes = after.variant.isIrreversible(this)
      val basePositionHashes =
        if resetsPositionHashes then PositionHash.empty else positionHashesOfBoardBefore
      h.copy(positionHashes = PositionHash(Hash(after)).combine(basePositionHashes))
    }

  private def castleRights: (Castles, UnmovedRooks) =
    var castleRights: Castles      = after.history.castles
    var unmovedRooks: UnmovedRooks = after.history.unmovedRooks

    // if the rook is captured
    // remove the captured rook from unmovedRooks
    // check the captured rook's side and remove it from castlingRights
    if captures then
      unmovedRooks.side(dest) match
        case Some(result) =>
          unmovedRooks = unmovedRooks & ~dest.bl
          result match
            case Some(side) =>
              castleRights = castleRights.without(!piece.color, side)
            case None =>
              // There is only one unmovedrook left so just remove the color from castlingRights
              castleRights = castleRights.without(!piece.color)
        case _ =>

    // if a rook is moved
    // remove that rook from unmovedRooks
    // check the moved rook's side and remove it from castlingRights
    if piece.is(Rook) && unmovedRooks.contains(orig) then
      unmovedRooks.side(orig) match
        case Some(result) =>
          unmovedRooks = unmovedRooks & ~orig.bl
          result match
            case Some(side) =>
              castleRights = castleRights.without(piece.color, side)
            case None =>
              // There is only one unmovedrook left so just remove the color from castlingRights
              castleRights = castleRights.without(piece.color)
        case _ =>

    // If the King is moved
    // remove castlingRights and unmovedRooks for the moving side
    else if piece.is(King) then
      unmovedRooks = unmovedRooks.without(piece.color)
      castleRights = castleRights.without(piece.color)

    (castleRights, unmovedRooks)

  // does this move capture an opponent piece?
  inline def captures: Boolean = capture.isDefined

  inline def promotes: Boolean = promotion.isDefined

  inline def castles: Boolean = castle.isDefined

  inline def normalizeCastle: Move =
    castle.fold(this)(x => copy(dest = x.rook))

  val isWhiteTurn: Boolean = piece.color.white
  inline def color         = piece.color

  def withPromotion(op: Option[PromotableRole]): Option[Move] =
    op.fold(this.some)(withPromotion)

  def withPromotion(p: PromotableRole): Option[Move] =
    if after.count(color.queen) > boardBefore.count(color.queen) then
      for
        b2 <- after.board.take(dest)
        b3 <- b2.put(color - p, dest)
      yield copy(after = after.withBoard(b3), promotion = Option(p))
    else this.some

  inline def withMetrics(m: MoveMetrics): Move = copy(metrics = m)

  override lazy val toSanStr: SanStr = format.pgn.Dumper(this)
  override lazy val toUci: Uci.Move  = Uci.Move(orig, dest, promotion)

  override def toString = s"$piece ${toUci.uci}"
end Move

object Move:

  case class Castle(king: Square, kingTo: Square, rook: Square, rookTo: Square):
    def side: Side          = if kingTo.file == File.C then QueenSide else KingSide
    def isStandard: Boolean = king.file == File.E && (rook.file == File.A || rook.file == File.H)

case class Drop(
    piece: Piece,
    square: Square,
    boardBefore: Position,
    after: Position,
    metrics: MoveMetrics = MoveMetrics.empty
) extends MoveOrDrop:

  override lazy val finalizeAfter: Position =
    val after = this.after.withColor(!piece.color)
    after
      .updateHistory { h =>
        val basePositionHashes =
          if h.positionHashes.value.isEmpty then PositionHash(Hash(boardBefore)) else h.positionHashes
        h.copy(
          lastMove = toUci.some,
          unmovedRooks = boardBefore.unmovedRooks,
          halfMoveClock = if piece.is(Pawn) then HalfMoveClock.initial else h.halfMoveClock.incr,
          positionHashes = PositionHash(Hash(after)).combine(basePositionHashes)
        )
      }

  inline def withMetrics(m: MoveMetrics): Drop = copy(metrics = m)

  override inline def color: Color   = piece.color
  override lazy val toSanStr: SanStr = format.pgn.Dumper(this)
  override lazy val toUci: Uci.Drop  = Uci.Drop(piece.role, square)

  override def toString = toUci.uci
