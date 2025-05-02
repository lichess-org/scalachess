package chess
package variant

import chess.format.FullFen

case object RacingKings
    extends Variant(
      id = Variant.Id(9),
      key = Variant.LilaKey("racingKings"),
      uciKey = Variant.UciKey("racingkings"),
      name = "Racing Kings",
      shortName = "Racing",
      title = "Race your King to the eighth rank to win.",
      standardInitialPosition = false
    ):

  // Both sides start on the first two ranks:
  // krbnNBRK
  // qrbnNBRQ
  override val pieces: Map[Square, Piece] = Map(
    Square.A1 -> Black.queen,
    Square.A2 -> Black.king,
    Square.B1 -> Black.rook,
    Square.B2 -> Black.rook,
    Square.C1 -> Black.bishop,
    Square.C2 -> Black.bishop,
    Square.D1 -> Black.knight,
    Square.D2 -> Black.knight,
    Square.E1 -> White.knight,
    Square.E2 -> White.knight,
    Square.F1 -> White.bishop,
    Square.F2 -> White.bishop,
    Square.G1 -> White.rook,
    Square.G2 -> White.rook,
    Square.H1 -> White.queen,
    Square.H2 -> White.king
  )

  override val castles: Castles        = Castles.none
  override val allowsCastling: Boolean = false

  override val initialFen: FullFen = FullFen("8/8/8/8/8/8/krbnNBRK/qrbnNBRQ w - - 0 1")

  override def validMoves(position: Position): List[Move] =
    import position.{ genSafeKing, genNonKingAndNonPawn, us }
    val targets = ~us
    val moves   = genNonKingAndNonPawn(targets) ++ genSafeKing(targets)
    moves.filter(kingSafety)

  override def valid(position: Position, strict: Boolean): Boolean =
    super.valid(position, strict) && (!strict || position.check.no)

  override def isInsufficientMaterial(position: Position): Boolean          = false
  override def opponentHasInsufficientMaterial(position: Position): Boolean = false

  private def reachedGoal(board: Board, color: Color): Boolean =
    board.kingOf(color).intersects(Bitboard.rank(Rank.Eighth))

  private def reachesGoal(move: Move) =
    reachedGoal(move.boardAfter.board, move.piece.color)

  // It is a win, when exactly one king made it to the goal. When white reaches
  // the goal and black can make it on the next ply, he is given a chance to
  // draw, to compensate for the first-move advantage. The draw is not called
  // automatically, because black should also be given equal chances to flag.
  override def specialEnd(position: Position): Boolean =
    position.color match
      case White =>
        reachedGoal(position.board, White) ^ reachedGoal(position.board, Black)
      case Black =>
        reachedGoal(position.board, White) && position.legalMoves.filter(reachesGoal).isEmpty

  // If white reaches the goal and black also reaches the goal directly after,
  // then it is a draw.
  override def specialDraw(position: Position): Boolean =
    position.color.white && reachedGoal(position.board, White) && reachedGoal(position.board, Black)

  override def winner(position: Position): Option[Color] =
    specialEnd(position).option(Color.fromWhite(reachedGoal(position.board, White)))

  // Not only check that our king is safe,
  // but also check the opponent's
  override def kingSafety(m: Move): Boolean =
    super.kingSafety(m) && m.after.isCheck(!m.color).no

  // When considering stalemate, take into account that checks are not allowed.
  override def staleMate(position: Position): Boolean =
    position.check.no && !specialEnd(position) && position.legalMoves.isEmpty
