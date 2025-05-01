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

  def validMoves(board: Board): List[Move] =
    import board.{ genNonKing, genSafeKing, genCastling, color, ourKing }
    val enPassantMoves = board.genEnPassant(board.us & board.pawns)
    ourKing.fold(Nil): king =>
      val checkers = board.attackers(king, !board.color)
      val candidates =
        if checkers.isEmpty then
          val targets = ~board.us
          genNonKing(targets) ::: genSafeKing(king, targets) ::: genCastling(king) ::: enPassantMoves
        else genEvasions(king, board, checkers) ::: enPassantMoves
      val sliderBlockers = board.sliderBlockers(king, color)
      if sliderBlockers.nonEmpty || enPassantMoves.nonEmpty then
        candidates.filter(isSafe(board, king, sliderBlockers))
      else candidates

  // Used for filtering candidate moves that would leave put the king in check.
  def isSafe(board: Board, king: Square, blockers: Bitboard)(move: Move): Boolean =
    import board.{ us, them }
    if move.enpassant then
      val newOccupied = (board.occupied ^ move.orig.bl ^ move.dest.withRankOf(move.orig).bl) | move.dest.bl
      (king.rookAttacks(newOccupied) & them & (board.rooks ^ board.queens)).isEmpty &&
      (king.bishopAttacks(newOccupied) & them & (board.bishops ^ board.queens)).isEmpty
    else if !move.castles || !move.promotes then
      !(us & blockers).contains(move.orig) || Bitboard.aligned(move.orig, move.dest, king)
    else true

  private def genEvasions(king: Square, board: Board, checkers: Bitboard): List[Move] =
    import board.{ genNonKing, genSafeKing, us }
    // Checks by these sliding pieces can maybe be blocked.
    val sliders   = checkers & board.sliders
    val attacked  = sliders.fold(Bitboard.empty)((a, s) => a | (Bitboard.ray(king, s) ^ s.bl))
    val safeKings = genSafeKing(king, ~us & ~attacked)
    val blockers  = checkers.singleSquare.fold(Nil)(c => genNonKing(Bitboard.between(king, c) | checkers))
    safeKings ++ blockers

  override def valid(board: Board, strict: Boolean): Boolean =
    super.valid(board, strict) && (!strict || hasValidCheckers(board))

  def hasValidCheckers(board: Board): Boolean =
    board.checkers.isEmpty || {
      isValidChecksForMultipleCheckers(board, board.checkers) &&
      isValidCheckersForEnPassant(board, board.checkers)
    }

  private def isValidCheckersForEnPassant(board: Board, activeCheckers: Bitboard): Boolean =
    (for
      enPassantSquare <- board.potentialEpSquare
      enPassantUp     <- board.color.fold(enPassantSquare.down, enPassantSquare.up)
      enPassantDown   <- board.color.fold(enPassantSquare.up, enPassantSquare.down)
      ourKing         <- board.ourKing
    yield activeCheckers.count == 1 && (
      activeCheckers.first.contains(enPassantSquare) || board
        .move(enPassantUp, enPassantDown)
        .exists(previousBoard => board.ourKing.exists(previousBoard.attackers(_, !board.color).isEmpty))
    )).getOrElse(true)

  private def isValidChecksForMultipleCheckers(board: Board, activeCheckers: Bitboard): Boolean =
    val checkerCount = activeCheckers.count
    if checkerCount <= 1 then true
    else if checkerCount >= 3 then false
    else
      (for
        firstChecker <- activeCheckers.first
        lastChecker  <- activeCheckers.last
        ourKing      <- board.ourKing
      yield !Bitboard.aligned(firstChecker, lastChecker, ourKing))
        .getOrElse(false)
