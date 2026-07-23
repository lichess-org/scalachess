package chess
package variant

import chess.format.FullFen

case object Antichess
    extends Variant(
      id = Variant.Id(6),
      key = Variant.LilaKey("antichess"),
      uciKey = Variant.UciKey("antichess"),
      name = "Antichess",
      shortName = "Anti",
      title = "Lose all your pieces (or get stalemated) to win the game.",
      standardInitialPosition = true
    ):

  override val initialBoard: Board = Board.standard
  override def initialPieces: Map[Square, Piece] = initialBoard.pieceMap

  // In antichess, it is not permitted to castle
  override val castles: Castles = Castles.none
  override val initialFen: FullFen = FullFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")

  // In antichess, the king can't be put into check so we always return false
  override def kingSafety(m: Move): Boolean = true

  override def kingThreatened(board: Board, color: Color): Check = Check.No

  override def validMoves(position: Position): List[Move] =
    import position.{ genNonKing, genUnsafeKing, ourKings }
    val capturingMoves = captureMoves(position)
    if capturingMoves.nonEmpty then capturingMoves
    else genNonKing(~position.occupied) ++ ourKings.flatMap(genUnsafeKing(_, ~position.occupied))

  override def validMovesAt(position: Position, square: Square): List[Move] =
    val captures = captureMoves(position)
    if captures.nonEmpty then captures.filter(_.orig == square)
    else super.validMovesAt(position, square)

  override def valid(position: Position, strict: Boolean): Boolean =
    position.nbPieces >= 2 && position.nbPieces <= 32

  // In antichess, there is no checkmate condition, and the winner is the current player if they have no legal moves
  override def winner(position: Position): Option[Color] =
    specialEnd(position).option(position.color)

  override def specialEnd(position: Position): Boolean =
    // The game ends with a win when one player manages to lose all their pieces or is in stalemate
    position.us.isEmpty || position.legalMoves.isEmpty

  // In antichess, it is valuable for your opponent to have pieces.
  override def materialImbalance(board: Board): Int =
    board.fold(0): (acc, color, _) =>
      acc + color.fold(-2, 2)

  // In antichess, if the only remaining pieces are a knight each, then exactly one
  // player can win (depending on whose turn it is).

  override def opponentHasInsufficientMaterial(position: Position): Boolean =
    hasInsufficientMaterial(position, !position.color)

  override def playerHasInsufficientMaterial(position: Position): Boolean =
    hasInsufficientMaterial(position, position.color)

  private def hasInsufficientMaterial(position: Position, color: Color): Boolean =
    (
      position.onlyKnights &&
        position.white.count == 1 &&
        position.black.count == 1 &&
        position.isTurn(color) != allOnSameColourSquares(position)
    ) ||
      {
        val subject = if position.isTurn(color) then position.us else position.them
        val opposing = if position.isTurn(color) then position.them else position.us
        val subjectBishops = subject & position.bishops
        val subjectPawns = subject & position.pawns
        val opposingBishops = opposing & position.bishops
        subjectBishops.nonEmpty &&
        opposingBishops.nonEmpty &&
        opposingBishops == opposing &&
        List(Bitboard.lightSquares, Bitboard.darkSquares).exists(colorComplex =>
          opposingBishops.isDisjoint(colorComplex) && subjectBishops.intersects(colorComplex)
        ) &&
        (
          subjectPawns.isEmpty || (
            // TODO: handle cases with > 1 subject pawn/opposing bishop that are still
            // impossible for the subject to win.
            subjectPawns.count == 1 &&
              opposingBishops.count == 1 &&
              List(
                (File.B, if color == Color.White then Bitboard.darkSquares else Bitboard.lightSquares),
                (File.G, if color == Color.White then Bitboard.lightSquares else Bitboard.darkSquares)
              ).forall: (file, colorComplex) =>
                subjectPawns.isDisjoint(Bitboard.file(file)) || opposingBishops.isDisjoint(colorComplex)
          )
        )
      }

  // No player can win if the only remaining pieces are opposing bishops on different coloured
  // diagonals. There may be pawns that are incapable of moving and do not attack the right color
  // of square to allow the player to force their opponent to capture their bishop, also resulting in a draw
  override def isInsufficientMaterial(position: Position): Boolean =
    // Exit early if we are not in a board with only bishops and pawns
    if (position.bishops | position.pawns) != position.occupied then false
    else
      val whiteBishops = (position.white & position.bishops).squares
      val blackBishops = (position.black & position.bishops).squares
      if whiteBishops.map(_.isLight).to(Set).size != 1 ||
        blackBishops.map(_.isLight).to(Set).size != 1
      then false
      else
        val whitePawns = (position.white & position.pawns).squares
        val blackPawns = (position.black & position.pawns).squares
        (for
          whiteBishopLight <- whiteBishops.headOption.map(_.isLight)
          blackBishopLight <- blackBishops.headOption.map(_.isLight)
        yield whiteBishopLight != blackBishopLight && whitePawns.forall(
          pawnNotAttackable(_, blackBishopLight, position)
        ) && blackPawns.forall(pawnNotAttackable(_, whiteBishopLight, position)))
          .getOrElse(false)

  // In this game variant, a king is a valid promotion
  override def isValidPromotion(_promotion: Option[PromotableRole]): Boolean = true

  override val promotableRoles: List[PromotableRole] = List(Queen, Rook, Bishop, Knight, King)

  private def captureMoves(position: Position): List[Move] =
    import position.{ them, us, genNonKing, genEnPassant, genUnsafeKing, ourKings }
    ourKings.flatMap(genUnsafeKing(_, them)) ++ genEnPassant(us & position.pawns) ++ genNonKing(them)

  private def pawnNotAttackable(pawn: Square, oppositeBishopLight: Boolean, position: Position): Boolean =
    // The pawn cannot attack a bishop or be attacked by a bishop
    val cannotAttackBishop = pawn.isLight != oppositeBishopLight
    InsufficientMatingMaterial.pawnBlockedByPawn(pawn, position) && cannotAttackBishop

  private def allOnSameColourSquares(position: Position): Boolean =
    Bitboard.lightSquares.isDisjoint(position.occupied) ||
      Bitboard.darkSquares.isDisjoint(position.occupied)
