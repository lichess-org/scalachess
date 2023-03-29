package chess
package variant

import bitboard.Bitboard
import bitboard.Bitboard.*

case object Standard
    extends Variant(
      id = Variant.Id(1),
      key = Variant.LilaKey("standard"),
      uciKey = Variant.UciKey("chess"),
      name = "Standard",
      shortName = "Std",
      title = "Standard rules of chess (FIDE)",
      standardInitialPosition = true
    ):

  val pieces: Map[Pos, Piece] = Variant.symmetricRank(backRank)

  def validMoves(situation: Situation): List[Move] =
    import situation.{ genNonKing, genSafeKing, genCastling, color, board }
    val enPassantMoves = situation.genEnPassant(situation.us & board.pawns)
    board
      .kingPosOf(color)
      .fold(Nil)(king =>
        val checkers = board.attackers(king, !situation.color)
        val candidates =
          if checkers.isEmpty then
            val targets = ~situation.us
            genNonKing(targets) ::: genSafeKing(targets) ::: genCastling(king) ::: enPassantMoves
          else genEvasions(situation, checkers) ::: enPassantMoves
        if situation.sliderBlockers.nonEmpty || enPassantMoves.nonEmpty then
          candidates.filter(isSafe(situation, king, _, situation.sliderBlockers))
        else candidates
      )

  // Used for filtering candidate moves that would leave put the king in check.
  def isSafe(situation: Situation, king: Pos, move: Move, blockers: Bitboard): Boolean =
    import situation.{ board, us, them }
    if move.enpassant then
      val newOccupied = (board.occupied ^ move.orig.bb ^ move.dest.withRankOf(move.orig).bb) | move.dest.bb
      (king.rookAttacks(newOccupied) & them & (board.rooks ^ board.queens)).isEmpty &&
      (king.bishopAttacks(newOccupied) & them & (board.bishops ^ board.queens)).isEmpty
    else if !move.castles || !move.promotes then
      !(us & blockers).contains(move.orig) || Bitboard.aligned(move.orig, move.dest, king)
    else true

  private def genEvasions(situation: Situation, checkers: Bitboard): List[Move] =
    import situation.{ genNonKing, genSafeKing, us, board, ourKing }
    ourKing.fold(Nil)(king =>
      // Checks by these sliding pieces can maybe be blocked.
      val sliders = checkers & (board.sliders)
      val attacked =
        sliders.squares.foldLeft(Bitboard.empty)((a, s) => a | (s.bb ^ Bitboard.ray(king, s)))
      val safeKings = genSafeKing(~us & ~attacked)
      val blockers =
        checkers.singleSquare.map(c => genNonKing(Bitboard.between(king, c) | checkers)).getOrElse(Nil)
      safeKings ++ blockers
    )
