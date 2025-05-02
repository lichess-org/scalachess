package chess
package variant

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

  override val pieces: Map[Square, Piece] = Variant.symmetricRank(backRank)

  override def valid(position: Position, strict: Boolean): Boolean =
    super.valid(position, strict) && (!strict || hasValidCheckers(position))

  override def validMoves(position: Position): List[Move] =
    import position.{ genNonKing, genSafeKing, genCastling, color, ourKing }
    val enPassantMoves = position.genEnPassant(position.us & position.pawns)
    ourKing.fold(Nil): king =>
      val checkers = position.attackers(king, !position.color)
      val candidates =
        if checkers.isEmpty then
          val targets = ~position.us
          genNonKing(targets) ::: genSafeKing(king, targets) ::: genCastling(king) ::: enPassantMoves
        else genEvasions(king, position, checkers) ::: enPassantMoves
      val sliderBlockers = position.sliderBlockers(king, color)
      if sliderBlockers.nonEmpty || enPassantMoves.nonEmpty then
        candidates.filter(isSafe(position, king, sliderBlockers))
      else candidates

  override def validMovesAt(position: Position, square: Square): List[Move] =
    super.validMovesAt(position, square).filter(kingSafety)

  // Used for filtering candidate moves that would leave put the king in check.
  def isSafe(position: Position, king: Square, blockers: Bitboard)(move: Move): Boolean =
    import position.{ us, them }
    if move.enpassant then
      val newOccupied = (position.occupied ^ move.orig.bl ^ move.dest.withRankOf(move.orig).bl) | move.dest.bl
      (king.rookAttacks(newOccupied) & them & (position.rooks ^ position.queens)).isEmpty &&
      (king.bishopAttacks(newOccupied) & them & (position.bishops ^ position.queens)).isEmpty
    else if !move.castles || !move.promotes then
      !(us & blockers).contains(move.orig) || Bitboard.aligned(move.orig, move.dest, king)
    else true

  private def genEvasions(king: Square, position: Position, checkers: Bitboard): List[Move] =
    import position.{ genNonKing, genSafeKing, us }
    // Checks by these sliding pieces can maybe be blocked.
    val sliders   = checkers & position.sliders
    val attacked  = sliders.fold(Bitboard.empty)((a, s) => a | (Bitboard.ray(king, s) ^ s.bl))
    val safeKings = genSafeKing(king, ~us & ~attacked)
    val blockers  = checkers.singleSquare.fold(Nil)(c => genNonKing(Bitboard.between(king, c) | checkers))
    safeKings ++ blockers

  def hasValidCheckers(position: Position): Boolean =
    position.checkers.isEmpty || {
      isValidChecksForMultipleCheckers(position, position.checkers) &&
      isValidCheckersForEnPassant(position, position.checkers)
    }

  private def isValidCheckersForEnPassant(position: Position, activeCheckers: Bitboard): Boolean =
    (for
      enPassantSquare <- position.potentialEpSquare
      enPassantUp     <- position.color.fold(enPassantSquare.down, enPassantSquare.up)
      enPassantDown   <- position.color.fold(enPassantSquare.up, enPassantSquare.down)
      ourKing         <- position.ourKing
    yield activeCheckers.count == 1 && (
      activeCheckers.first.contains(enPassantSquare) || position.board
        .move(enPassantUp, enPassantDown)
        .exists(previousBoard => position.ourKing.exists(previousBoard.attackers(_, !position.color).isEmpty))
    )).getOrElse(true)

  private def isValidChecksForMultipleCheckers(position: Position, activeCheckers: Bitboard): Boolean =
    val checkerCount = activeCheckers.count
    if checkerCount <= 1 then true
    else if checkerCount >= 3 then false
    else
      (for
        firstChecker <- activeCheckers.first
        lastChecker  <- activeCheckers.last
        ourKing      <- position.ourKing
      yield !Bitboard.aligned(firstChecker, lastChecker, ourKing))
        .getOrElse(false)
