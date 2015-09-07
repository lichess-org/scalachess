package chess
package variant

case object RacingKings extends Variant(
  id = 9,
  key = "racingKings",
  name = "Racing Kings",
  shortName = "Racing",
  title = "Race to the eighth rank to win.",
  standardInitialPosition = false) {

  override val drawsOnInsufficientMaterial = false

  override def allowsCastling = false

  // Both sides start on the first two ranks:
  // krbnNBRK
  // qrbnNBRQ
  override lazy val pieces: Map[Pos, Piece] = Map(
    Pos.A1 -> Black.queen,
    Pos.A2 -> Black.king,
    Pos.B1 -> Black.rook,
    Pos.B2 -> Black.rook,
    Pos.C1 -> Black.bishop,
    Pos.C2 -> Black.bishop,
    Pos.D1 -> Black.knight,
    Pos.D2 -> Black.knight,
    Pos.E1 -> White.knight,
    Pos.E2 -> White.knight,
    Pos.F1 -> White.bishop,
    Pos.F2 -> White.bishop,
    Pos.G1 -> White.rook,
    Pos.G2 -> White.rook,
    Pos.H1 -> White.queen,
    Pos.H2 -> White.king
  )

  // The goal for the kings is the eighth rank.
  private val goal = Pos.A8 <-> Pos.H8 toSet

  private def reachedGoal(situation: Situation, color: Color) =
    situation.board.kingPosOf(color) exists goal.contains

  private def reachesGoal(move: Move) =
    reachedGoal(move.situationAfter, move.piece.color)

  // It is a win, when exactly one king made it to the goal. When white reaches
  // the goal and black can make it on the next ply, he is given a chance to
  // draw, to compensate for the first-move advantage. The draw is not called
  // automatically, because black should also be given equal chances to flag.
  override def specialEnd(situation: Situation) = {
    situation.color match {
      case White =>
        reachedGoal(situation, White) ^ reachedGoal(situation, Black)
      case Black =>
        reachedGoal(situation, White) && (validMoves(situation) mapValues (_.filter(reachesGoal))).forall(_._2.isEmpty)
    }
  }

  // If white reaches the goal and black also reaches the goal directly after,
  // then it is a draw.
  override def specialDraw(situation: Situation) =
    situation.color.white && reachedGoal(situation, White) && reachedGoal(situation, Black)

  override def winner(situation: Situation): Option[Color] = {
    if (specialEnd(situation)) {
      if (reachedGoal(situation, White)) Some(White) else Some(Black)
    } else None
  }

  private def givesCheck(move: Move) =
    move.situationAfter.check

  // Giving check is not allowed.
  override def validMoves(situation: Situation) =
    super.validMoves(situation) mapValues (_.filterNot(givesCheck))

  // When considering stalemate, take into account that checks are not allowed.
  override def staleMate(situation: Situation): Boolean =
    !situation.check && !specialEnd(situation) && !validMoves(situation).exists(_._2.nonEmpty)
}
