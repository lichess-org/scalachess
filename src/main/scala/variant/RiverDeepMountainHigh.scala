package chess
package variant

import chess.File._
import chess.Rank._

case object RiverDeepMountainHigh
    extends Variant(
      id = 10,
      key = "river-deep-mountain-high",
      name = "River Deep Mountain High",
      shortName = "RD/MH",
      title =
        "Knights can't move to the sides of the board (river). Rooks can't move across the 4 centre squares (mountain). Created by Lindosa & Araujo",
      standardInitialPosition = true
    ) {

  def pieces = Standard.pieces

  // In this variant, Knights can't move to the sides and Rooks can't move across the 4 centre squares
  override def validMoves(situation: Situation) = {
    val allMoves = super.validMoves(situation)
    val filteredMoves = allMoves.view mapValues (_.filterNot(move => {
      KnightIsBlockedByRiver(move) || RookIsBlockedByMountain(move)
    }))
    filteredMoves.to(Map)
  }

  private def KnightIsBlockedByRiver(m: Move) = (m.piece is Knight) && isRiver(m.dest)

  private def RookIsBlockedByMountain(m: Move) = (m.piece is Rook) && blockedByMountain(m.orig, m.dest)

  private def isRiver(pos: Pos): Boolean =
    Set(A, H).contains(pos.file) || Set(First, Eighth).contains(pos.rank)

  private def blockedByMountain(from: Pos, to: Pos): Boolean =
    if (from ?- to && (Set(Fourth, Fifth) contains from.rank)) {                       // horizontal movement
      (from.file < F && C < to.file) || (to.file < F && C < from.file)                 // mountain between from and to
    } else if (from ?| to && (Set(D, E) contains from.file)) {                         // vertical movement
      (from.rank < Sixth && Third < to.rank) || (to.rank < Sixth && Third < from.rank) // mountain between from and to
    } else false
}
