package chess

import format.{ pgn, Uci }

case class Game(
    board: Board,
    player: Color = White,
    pgnMoves: Vector[String] = Vector(),
    clock: Option[Clock] = None,
    turns: Int = 0, // plies
    startedAtTurn: Int = 0
) {

  def apply(
    orig: Pos,
    dest: Pos,
    promotion: Option[PromotableRole] = None,
    metrics: MoveMetrics = MoveMetrics()
  ): Valid[(Game, Move)] =
    situation.move(orig, dest, promotion).map(_.normalizeCastle withMetrics metrics) map { move =>
      apply(move) -> move
    }

  def apply(move: Move): Game = {
    val newGame = copy(
      board = move.finalizeAfter,
      player = !player,
      turns = turns + 1
    )
    newGame.copy(
      pgnMoves = pgnMoves :+ pgn.Dumper(situation, move, newGame.situation),
      clock = applyClock(move.metrics, newGame.situation.status.isEmpty)
    )
  }

  def drop(
    role: Role,
    pos: Pos,
    metrics: MoveMetrics = MoveMetrics()
  ): Valid[(Game, Drop)] =
    situation.drop(role, pos).map(_ withMetrics metrics) map { drop =>
      applyDrop(drop) -> drop
    }

  def applyDrop(drop: Drop): Game = {
    val newGame = copy(
      board = drop.finalizeAfter,
      player = !player,
      turns = turns + 1
    )
    newGame.copy(
      pgnMoves = pgnMoves :+ pgn.Dumper(drop, newGame.situation),
      clock = applyClock(drop.metrics, newGame.situation.status.isEmpty)
    )
  }

  private def applyClock(metrics: MoveMetrics, withInc: Boolean) = clock.map { c =>
    c.step(metrics, withInc) getOrElse {
      if (turns - startedAtTurn == 1) c.switch.start
      else c.switch
    }
  }

  def apply(uci: Uci.Move): Valid[(Game, Move)] = apply(uci.orig, uci.dest, uci.promotion)
  def apply(uci: Uci.Drop): Valid[(Game, Drop)] = drop(uci.role, uci.pos)
  def apply(uci: Uci): Valid[(Game, MoveOrDrop)] = uci match {
    case u: Uci.Move => apply(u) map { case (g, m) => g -> Left(m) }
    case u: Uci.Drop => apply(u) map { case (g, d) => g -> Right(d) }
  }

  lazy val situation = Situation(board, player)

  def isStandardInit = board.pieces == chess.variant.Standard.pieces

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

  def apply(variantOption: Option[chess.variant.Variant], fen: Option[String]): Game = {
    val variant = variantOption | chess.variant.Standard
    val g = apply(variant)
    fen.flatMap {
      format.Forsyth.<<<@(variant, _)
    }.fold(g) { parsed =>
      g.copy(
        board = parsed.situation.board withVariant g.board.variant withCrazyData {
        parsed.situation.board.crazyData orElse g.board.crazyData
      },
        player = parsed.situation.color,
        turns = parsed.turns
      )
    }
  }
}
