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

  val pieces: Map[Square, Piece] = Variant.symmetricRank(backRank)

  def validMoves(situation: Situation): List[Move] =
    import situation.{ genNonKing, genSafeKing, genCastling, color, board, ourKing }
    val enPassantMoves = situation.genEnPassant(situation.us & board.pawns)
    ourKing
      .fold(Nil): king =>
        val checkers = board.attackers(king, !situation.color)
        val candidates =
          if checkers.isEmpty then
            val targets = ~situation.us
            genNonKing(targets) ::: genSafeKing(king, targets) ::: genCastling(king) ::: enPassantMoves
          else genEvasions(king, situation, checkers) ::: enPassantMoves
        val sliderBlockers = board.sliderBlockers(king, color)
        if sliderBlockers.nonEmpty || enPassantMoves.nonEmpty then
          candidates.filter(isSafe(situation, king, sliderBlockers))
        else candidates

  override def hasValidCheckers(strict: Boolean, situation: Situation): Boolean =
    !strict || situation.checkers.isEmpty || {
      isValidChecksForMultipleCheckers(situation, situation.checkers) &&
      isValidCheckersForEnPassant(situation, situation.checkers)
    }

  private def isValidCheckersForEnPassant(situation: Situation, activeCheckers: Bitboard): Boolean =
    (for
      enPassantSquare <- situation.potentialEpSquare
      enPassantUp     <- situation.color.fold(enPassantSquare.down, enPassantSquare.up)
      enPassantDown   <- situation.color.fold(enPassantSquare.up, enPassantSquare.down)
      ourKing         <- situation.ourKing
    yield activeCheckers.count == 1 && (
      activeCheckers.first.contains(enPassantSquare) || situation.board
        .move(enPassantUp, enPassantDown)
        .exists(previousBoard =>
          situation.ourKing.exists(previousBoard.attackers(_, !situation.color).isEmpty)
        )
    )).getOrElse(true)

  private def isValidChecksForMultipleCheckers(situation: Situation, activeCheckers: Bitboard): Boolean =
    val checkerCount = activeCheckers.count
    if checkerCount <= 1 then true
    else if checkerCount >= 3 then false
    else
      (for
        firstChecker <- activeCheckers.first
        lastChecker  <- activeCheckers.last
        ourKing      <- situation.ourKing
      yield !Bitboard.aligned(firstChecker, lastChecker, ourKing))
        .getOrElse(false)

  // Used for filtering candidate moves that would leave put the king in check.
  def isSafe(situation: Situation, king: Square, blockers: Bitboard)(move: Move): Boolean =
    import situation.{ board, us, them }
    if move.enpassant then
      val newOccupied = (board.occupied ^ move.orig.bl ^ move.dest.withRankOf(move.orig).bl) | move.dest.bl
      (king.rookAttacks(newOccupied) & them & (board.rooks ^ board.queens)).isEmpty &&
      (king.bishopAttacks(newOccupied) & them & (board.bishops ^ board.queens)).isEmpty
    else if !move.castles || !move.promotes then
      !(us & blockers).contains(move.orig) || Bitboard.aligned(move.orig, move.dest, king)
    else true

  private def genEvasions(king: Square, situation: Situation, checkers: Bitboard): List[Move] =
    import situation.{ genNonKing, genSafeKing, us, board }
    // Checks by these sliding pieces can maybe be blocked.
    val sliders   = checkers & board.sliders
    val attacked  = sliders.fold(Bitboard.empty)((a, s) => a | (Bitboard.ray(king, s) ^ s.bl))
    val safeKings = genSafeKing(king, ~us & ~attacked)
    val blockers  = checkers.singleSquare.fold(Nil)(c => genNonKing(Bitboard.between(king, c) | checkers))
    safeKings ++ blockers
