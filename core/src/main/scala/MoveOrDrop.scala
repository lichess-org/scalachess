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

  def finalizeAfter: Board

  def situationBefore: Situation

  def situationAfter: Situation

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
    situationBefore: Situation,
    after: Board,
    capture: Option[Square],
    promotion: Option[PromotableRole],
    castle: Option[Move.Castle],
    enpassant: Boolean,
    metrics: MoveMetrics = MoveMetrics.empty
) extends MoveOrDrop:

  inline def before         = situationBefore.board
  def situationAfter        = Situation(finalizeAfter, !piece.color)
  lazy val toSanStr: SanStr = format.pgn.Dumper(this)

  // TODO rethink about how handle castling
  // it's quite messy and error prone now
  lazy val finalizeAfter: Board =
    val board = after.variant.finalizeBoard(
      after.updateHistory { h1 =>
        val h2 = h1.copy(
          lastMove = Option(toUci),
          halfMoveClock =
            if piece.is(Pawn) || captures || promotes then HalfMoveClock.initial
            else h1.halfMoveClock.map(_ + 1)
        )

        var castleRights: Castles      = h1.castles
        var unmovedRooks: UnmovedRooks = h1.unmovedRooks

        // If the rook is captured
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

        // If a Rook is moved
        // Remove that rook from unmovedRooks.
        // check the captured rook's side and remove it from castlingRights
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

        h2.withCastles(castleRights).copy(unmovedRooks = unmovedRooks)
      },
      toUci,
      capture.flatMap(before(_))
    )

    // Update position hashes last, only after updating the board,
    // castling rights and en-passant rights.
    board.updateHistory { h =>
      lazy val positionHashesOfSituationBefore =
        if h.positionHashes.isEmpty then PositionHash(Hash(situationBefore)) else h.positionHashes
      val resetsPositionHashes = board.variant.isIrreversible(this)
      val basePositionHashes =
        if resetsPositionHashes then PositionHash.empty else positionHashesOfSituationBefore
      h.copy(positionHashes = PositionHash(Hash(Situation(board, !piece.color))).combine(basePositionHashes))
    }

  // does this move capture an opponent piece?
  inline def captures = capture.isDefined

  inline def promotes = promotion.isDefined

  inline def castles = castle.isDefined

  inline def normalizeCastle =
    castle.fold(this)(x => copy(dest = x.rook))

  val isWhiteTurn: Boolean = piece.color.white
  inline def color         = piece.color

  inline def withHistory(inline h: History) = copy(after = after.withHistory(h))

  def withPromotion(op: Option[PromotableRole]): Option[Move] =
    op.fold(this.some)(withPromotion)

  def withPromotion(p: PromotableRole): Option[Move] =
    if after.count(color.queen) > before.count(color.queen) then
      for
        b2 <- after.take(dest)
        b3 <- b2.place(color - p, dest)
      yield copy(after = b3, promotion = Option(p))
    else this.some

  inline def withAfter(newBoard: Board) = copy(after = newBoard)

  inline def withMetrics(m: MoveMetrics) = copy(metrics = m)

  inline def toUci: Uci.Move = Uci.Move(orig, dest, promotion)

  override def toString = s"$piece ${toUci.uci}"
end Move

object Move:

  case class Castle(king: Square, kingTo: Square, rook: Square, rookTo: Square):
    def side: Side          = if kingTo.file == File.C then QueenSide else KingSide
    def isStandard: Boolean = king.file == File.E && (rook.file == File.A || rook.file == File.H)

case class Drop(
    piece: Piece,
    square: Square,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics.empty
) extends MoveOrDrop:

  inline def before         = situationBefore.board
  def situationAfter        = Situation(finalizeAfter, !piece.color)
  lazy val toSanStr: SanStr = format.pgn.Dumper(this)

  lazy val finalizeAfter: Board =
    val board = after.variant.finalizeBoard(
      after.updateHistory { h =>
        h.copy(
          lastMove = Option(Uci.Drop(piece.role, square)),
          unmovedRooks = before.unmovedRooks,
          halfMoveClock = if piece.is(Pawn) then HalfMoveClock.initial else h.halfMoveClock.map(_ + 1)
        )
      },
      toUci,
      none
    )

    board.updateHistory { h =>
      val basePositionHashes =
        if h.positionHashes.value.isEmpty then PositionHash(Hash(situationBefore)) else h.positionHashes
      h.copy(positionHashes = PositionHash(Hash(Situation(board, !piece.color))).combine(basePositionHashes))
    }

  inline def withHistory(inline h: History) = copy(after = after.withHistory(h))

  def afterWithLastMove: Board =
    after.variant.finalizeBoard(
      after.copy(history = after.history.withLastMove(toUci)),
      toUci,
      none
    )

  inline def color = piece.color

  inline def withAfter(newBoard: Board) = copy(after = newBoard)

  inline def withMetrics(m: MoveMetrics) = copy(metrics = m)

  inline def toUci: Uci.Drop = Uci.Drop(piece.role, square)

  override def toString = toUci.uci
