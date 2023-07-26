package chess
package variant

import chess.format.EpdFen
import bitboard.Bitboard

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

  override val allowsCastling = false

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

  override val castles = Castles.none

  override val initialFen = EpdFen("8/8/8/8/8/8/krbnNBRK/qrbnNBRQ w - - 0 1")

  def validMoves(situation: Situation): List[Move] =
    import situation.{ genSafeKing, genNonKingAndNonPawn, us }
    val targets = ~us
    val moves   = genNonKingAndNonPawn(targets) ++ genSafeKing(targets)
    moves.filter(kingSafety)

  override def valid(situation: Situation, strict: Boolean): Boolean =
    super.valid(situation, strict) && (!strict || situation.check.no)

  override def isInsufficientMaterial(board: Board)                  = false
  override def opponentHasInsufficientMaterial(situation: Situation) = false

  private def reachedGoal(board: Board, color: Color) =
    board.kingOf(color).intersects(Bitboard.rank(Rank.Eighth))

  private def reachesGoal(move: Move) =
    reachedGoal(move.situationAfter.board, move.piece.color)

  // It is a win, when exactly one king made it to the goal. When white reaches
  // the goal and black can make it on the next ply, he is given a chance to
  // draw, to compensate for the first-move advantage. The draw is not called
  // automatically, because black should also be given equal chances to flag.
  override def specialEnd(situation: Situation) =
    situation.color match
      case White =>
        reachedGoal(situation.board, White) ^ reachedGoal(situation.board, Black)
      case Black =>
        reachedGoal(situation.board, White) && situation.legalMoves.filter(reachesGoal).isEmpty

  // If white reaches the goal and black also reaches the goal directly after,
  // then it is a draw.
  override def specialDraw(situation: Situation) =
    situation.color.white && reachedGoal(situation.board, White) && reachedGoal(situation.board, Black)

  override def winner(situation: Situation): Option[Color] =
    specialEnd(situation) option Color.fromWhite(reachedGoal(situation.board, White))

  // Not only check that our king is safe,
  // but also check the opponent's
  override def kingSafety(m: Move): Boolean =
    super.kingSafety(m) && m.after.isCheck(!m.color).no

  // When considering stalemate, take into account that checks are not allowed.
  override def staleMate(situation: Situation): Boolean =
    situation.check.no && !specialEnd(situation) && situation.legalMoves.isEmpty
