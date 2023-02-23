package chess

import chess.format.Uci
import bitboard.Bitboard.*
import cats.syntax.option.*
import cats.kernel.Monoid
import chess.bitboard.Bitboard

case class Move(
    piece: Piece,
    orig: Pos,
    dest: Pos,
    situationBefore: Situation,
    after: Board,
    capture: Option[Pos],
    promotion: Option[PromotableRole],
    castle: Option[Move.Castle],
    enpassant: Boolean,
    metrics: MoveMetrics = MoveMetrics.empty
):
  inline def before = situationBefore.board

  inline def situationAfter = Situation(finalizeAfter, !piece.color)

  inline def withHistory(inline h: History) = copy(after = after withHistory h)

  val isWhiteTurn: Boolean = piece.color.white

  // TODO rethink about how handle castling
  // it's quite messy and error prone now
  def finalizeAfter: Board =
    val board = after.variant.finalizeBoard(
      after updateHistory { h1 =>
        val h2 = h1.copy(
          lastMove = Option(toUci),
          unmovedRooks = before.unmovedRooks,
          halfMoveClock =
            if (piece.is(Pawn) || captures || promotes) HalfMoveClock(0)
            else h1.halfMoveClock + 1
        )

        var castleRights: Castles      = h2.castles
        var unmovedRooks: UnmovedRooks = h2.unmovedRooks

        // If the rook is captured
        // remove the captured rook from unmovedRooks
        // check the captured rook's side and remove it from castlingRights
        if captures then
          unmovedRooks.side(dest) match
            case Some(result) =>
              unmovedRooks = unmovedRooks & ~dest.bb
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
        if piece is Rook then
          unmovedRooks.side(orig) match
            case Some(result) =>
              unmovedRooks = unmovedRooks & ~orig.bb
              result match
                case Some(side) =>
                  castleRights = castleRights.without(piece.color, side)
                case None =>
                  // There is only one unmovedrook left so just remove the color from castlingRights
                  castleRights = castleRights.without(piece.color)
            case _ =>

        // If the King is moved
        // remove castlingRights and unmovedRooks for the moving side
        else if piece is King then
          unmovedRooks = unmovedRooks.without(piece.color)
          castleRights = castleRights.without(piece.color)

        h2.withCastles(castleRights).copy(unmovedRooks = unmovedRooks)
      },
      toUci,
      capture flatMap { before(_) }
    )

    // Update position hashes last, only after updating the board,
    // castling rights and en-passant rights.
    board updateHistory { h =>
      lazy val positionHashesOfSituationBefore =
        if (h.positionHashes.value.isEmpty) Hash(situationBefore) else h.positionHashes
      val resetsPositionHashes = board.variant.isIrreversible(this)
      val basePositionHashes =
        if (resetsPositionHashes) Monoid[PositionHash].empty else positionHashesOfSituationBefore
      h.copy(positionHashes =
        Monoid[PositionHash].combine(Hash(Situation(board, !piece.color)), basePositionHashes)
      )
    }

  def applyVariantEffect: Move = before.variant addVariantEffect this

  // does this move capture an opponent piece?
  inline def captures = capture.isDefined

  inline def promotes = promotion.isDefined

  inline def castles = castle.isDefined

  inline def normalizeCastle =
    Move.Castle.raw(castle).fold(this) { case (_, (rookOrig, _)) =>
      copy(dest = rookOrig)
    }

  inline def color = piece.color

  def withPromotion(op: Option[PromotableRole]): Option[Move] =
    op.fold(this.some)(withPromotion)

  def withPromotion(p: PromotableRole): Option[Move] =
    if (after.count(color.queen) > before.count(color.queen)) for {
      b2 <- after take dest
      b3 <- b2.place(color - p, dest)
    } yield copy(after = b3, promotion = Option(p))
    else this.some

  inline def withAfter(newBoard: Board) = copy(after = newBoard)

  inline def withMetrics(m: MoveMetrics) = copy(metrics = m)

  inline def toUci = Uci.Move(orig, dest, promotion)

  override def toString = s"$piece ${toUci.uci}"
end Move

object Move:

  // ((king, kingTo), (rook, rookTo))
  opaque type Castle = ((Pos, Pos), (Pos, Pos))
  object Castle extends TotalWrapper[Castle, ((Pos, Pos), (Pos, Pos))]:
    extension (e: Castle)
      inline def king         = e._1._1
      inline def kingTo       = e._1._2
      inline def rook         = e._2._1
      inline def rookTo       = e._2._2
      def side: Side          = if kingTo.file == File.C then QueenSide else KingSide
      def isStandard: Boolean = king.file == File.E && (rook.file == File.A || rook.file == File.H)
