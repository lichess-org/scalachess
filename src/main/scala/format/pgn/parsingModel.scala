package chess
package format.pgn

case class ParsedPgn(tags: List[Tag], sans: List[San])

// Standard Algebraic Notation
sealed trait San {

  def apply(game: Game): Valid[chess.Move]
}

case class Std(
    dest: Pos,
    role: Role,
    capture: Boolean = false,
    file: Option[Int] = None,
    rank: Option[Int] = None,
    check: Boolean = false,
    checkmate: Boolean = false,
    promotion: Option[PromotableRole] = None) extends San {

  def withSuffixes(s: Suffixes) = copy(
    check = s.check,
    checkmate = s.checkmate,
    promotion = s.promotion)

  def apply(game: Game): Valid[chess.Move] = {
    def compare[A](a: Option[A], b: => A) = a map (_ == b) getOrElse true
    game.situation.moves map {
      case (orig, moves) => moves find { move =>
        move.dest == dest && move.piece.role == role
      }
    } collect {
      case Some(m) if compare(file, m.orig.x) && compare(rank, m.orig.y) => m
    } match {
      case Nil        => s"No move found: $this\n${game.board}".failNel
      case one :: Nil => one withPromotion promotion toValid "Wrong promotion"
      case many       => s"Many moves found: $many\n${game.board}".failNel
    }
  }

  override def toString = role.forsyth + dest.toString
}

case class Suffixes(
  check: Boolean,
  checkmate: Boolean,
  promotion: Option[PromotableRole])

case class Castle(
    side: Side,
    check: Boolean = false,
    checkmate: Boolean = false) extends San {

  def withSuffixes(s: Suffixes) = copy(
    check = s.check,
    checkmate = s.checkmate)

  def apply(game: Game): Valid[chess.Move] = for {
    kingPos ← game.board kingPosOf game.player toValid "No king found"
    actor ← game.board actorAt kingPos toValid "No actor found"
    move ← actor.castleOn(side).headOption toValid "Cannot castle / variant is " + game.board.variant
  } yield move
}
