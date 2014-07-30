package chess

import scala.concurrent.duration._

case class Move(
    piece: Piece,
    orig: Pos,
    dest: Pos,
    before: Board,
    after: Board,
    capture: Option[Pos],
    promotion: Option[PromotableRole],
    castle: Option[((Pos, Pos), (Pos, Pos))],
    enpassant: Boolean,
    lag: FiniteDuration = 0.millis) {

  def situationBefore = before situationOf piece.color
  def situationAfter = finalizeAfter situationOf !piece.color

  def withHistory(h: History) = copy(after = after withHistory h)

  def finalizeAfter: Board = before.variant finalizeMove {
    after updateHistory { h1 =>
      // last move and position hashes
      val h2 = h1.copy(
        positionHashes =
          if ((piece is Pawn) || captures || promotes || castles) Array()
          else h1 positionHashesWith Hash(after.actors.values, piece.color),
        lastMove = Some(orig, dest)
      )
      // my broken castles
      val h3 =
        if ((piece is King) && h2.canCastle(color).any)
          h2 withoutCastles color
        else if (piece is Rook) (for {
          kingPos ← after kingPosOf color
          side ← Side.kingRookSide(kingPos, orig)
          if h2 canCastle color on side
        } yield h2.withoutCastle(color, side)) | h2
        else h2
      // opponent broken castles
      (for {
        cPos ← capture
        cPiece ← before(cPos)
        if cPiece is Rook
        kingPos ← after kingPosOf !color
        side ← Side.kingRookSide(kingPos, cPos)
        if h3 canCastle !color on side
      } yield h3.withoutCastle(!color, side)) | h3
    }
  }

  // does this move capture an opponent piece?
  def captures = capture.isDefined

  def promotes = promotion.isDefined

  def castles = castle.isDefined

  def color = piece.color

  def spaceNotation = orig + " " + dest

  def withPromotion(op: Option[PromotableRole]): Option[Move] = op.fold(some(this)) { p =>
    if ((after count color.queen) > (before count color.queen)) for {
      b2 ← after take dest
      b3 ← b2.place(color - p, dest)
    } yield copy(after = b3, promotion = Some(p))
    else None
  }

  def withLag(l: FiniteDuration) = copy(lag = l)

  def keyString = s"$orig$dest"

  override def toString = s"$piece $keyString"
}
