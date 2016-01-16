package chess

import scala.concurrent.duration._

import format.{ pgn, Uci }

case class Game(
    board: Board,
    player: Color = White,
    pgnMoves: List[String] = Nil,
    clock: Option[Clock] = None,
    turns: Int = 0,
    startedAtTurn: Int = 0) {

  def apply(
    orig: Pos,
    dest: Pos,
    promotion: Option[PromotableRole] = None,
    lag: FiniteDuration = 0.millis): Valid[(Game, Move)] =
    situation.move(orig, dest, promotion).map(_ withLag lag) map { move =>
      apply(move) -> move
    }

  def apply(move: Move): Game = {
    val newTurns = turns + 1
    val newGame = copy(
      board = move.finalizeAfter,
      player = !player,
      turns = newTurns,
      clock = clock map {
        case c: RunningClock => c step move.lag
        case c: PausedClock if (newTurns - startedAtTurn) == 2 => c.start.switch
        case c => c
      }
    )
    val pgnMove = pgn.Dumper(situation, move, newGame.situation)
    newGame.copy(pgnMoves = pgnMoves.isEmpty.fold(
      List(pgnMove),
      pgnMoves :+ pgnMove))
  }

  def drop(role: Role, pos: Pos, lag: FiniteDuration = 0.millis): Valid[(Game, Drop)] =
    situation.drop(role, pos).map(_ withLag lag) map { drop =>
      applyDrop(drop) -> drop
    }

  def applyDrop(drop: Drop): Game = {
    val newTurns = turns + 1
    val newGame = copy(
      board = drop.finalizeAfter,
      player = !player,
      turns = newTurns,
      clock = clock map {
        case c: RunningClock => c step drop.lag
        case c: PausedClock if (newTurns - startedAtTurn) == 2 => c.start.switch
        case c => c
      }
    )
    val pgnMove = pgn.Dumper(situation, drop, newGame.situation)
    newGame.copy(pgnMoves = pgnMoves.isEmpty.fold(
      List(pgnMove),
      pgnMoves :+ pgnMove))
  }

  def apply(uci: Uci.Move): Valid[(Game, Move)] = apply(uci.orig, uci.dest, uci.promotion)
  def apply(uci: Uci.Drop): Valid[(Game, Drop)] = drop(uci.role, uci.pos)
  def apply(uci: Uci): Valid[(Game, MoveOrDrop)] = uci match {
    case u: Uci.Move => apply(u) map { case (g, m) => g -> Left(m) }
    case u: Uci.Drop => apply(u) map { case (g, d) => g -> Right(d) }
  }

  lazy val situation = Situation(board, player)

  def isStandardInit = board.pieces == chess.variant.Standard.pieces

  def withPgnMoves(x: List[String]) = copy(pgnMoves = x)

  def halfMoveClock: Int = board.history.halfMoveClock

  /**
   * Fullmove number: The number of the full move.
   * It starts at 1, and is incremented after Black's move.
   */
  def fullMoveNumber: Int = 1 + turns / 2

  def withBoard(b: Board) = copy(board = b)

  def updateBoard(f: Board => Board) = withBoard(f(board))

  def withPlayer(c: Color) = copy(player = c)

  def withTurns(t: Int) = copy(turns = t)
}

object Game {

  def apply(variant: chess.variant.Variant): Game = new Game(
    board = Board init variant
  )

  def apply(variant: Option[chess.variant.Variant], fen: Option[String]): Game = {
    val g = apply(variant | chess.variant.Standard)
    fen.flatMap(format.Forsyth.<<<).fold(g) { parsed =>
      g.copy(
        board = parsed.situation.board withVariant g.board.variant,
        player = parsed.situation.color,
        turns = parsed.turns)
    }
  }
}
