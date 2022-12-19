package chess

import cats.data.Validated

import chess.format.Fen
import chess.format.{ pgn, Uci }
import chess.format.pgn.SanStr

case class Game(
    situation: Situation,
    sans: Vector[SanStr] = Vector(),
    clock: Option[Clock] = None,
    turns: Ply = Ply(0), // plies
    startedAtTurn: Ply = Ply(0)
):

  export situation.{ board, color as player }
  export situation.board.history.halfMoveClock

  def apply(
      orig: Pos,
      dest: Pos,
      promotion: Option[PromotableRole] = None,
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Move)] =
    moveWithCompensated(orig, dest, promotion, metrics).map { case (game, move) =>
      (game.value, move)
    }

  def moveWithCompensated(
      orig: Pos,
      dest: Pos,
      promotion: Option[PromotableRole] = None,
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Clock.WithCompensatedLag[Game], Move)] =
    situation.move(orig, dest, promotion).map(_.normalizeCastle withMetrics metrics) map { move =>
      applyWithCompensated(move) -> move
    }

  def apply(move: Move): Game = applyWithCompensated(move).value

  def applyWithCompensated(move: Move): Clock.WithCompensatedLag[Game] =
    val newSituation = move.situationAfter
    val newClock     = applyClock(move.metrics, newSituation.status.isEmpty)

    Clock.WithCompensatedLag(
      copy(
        situation = newSituation,
        turns = turns + 1,
        sans = sans :+ pgn.Dumper(situation, move, newSituation),
        clock = newClock.map(_.value)
      ),
      newClock.flatMap(_.compensated)
    )

  def drop(
      role: Role,
      pos: Pos,
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Drop)] =
    situation.drop(role, pos).map(_ withMetrics metrics) map { drop =>
      applyDrop(drop) -> drop
    }

  def applyDrop(drop: Drop): Game =
    val newSituation = drop situationAfter

    copy(
      situation = newSituation,
      turns = turns + 1,
      sans = sans :+ pgn.Dumper(drop, newSituation),
      clock = applyClock(drop.metrics, newSituation.status.isEmpty).map(_.value)
    )

  private def applyClock(
      metrics: MoveMetrics,
      gameActive: => Boolean
  ): Option[Clock.WithCompensatedLag[Clock]] =
    clock.map { prev =>
      {
        val c1 = metrics.frameLag.fold(prev)(prev.withFrameLag)
        val c2 = c1.step(metrics, gameActive)
        if (turns - startedAtTurn == Ply(1)) c2.map(_.start) else c2
      }
    }

  def apply(uci: Uci.Move): Validated[String, (Game, Move)] = apply(uci.orig, uci.dest, uci.promotion)
  def apply(uci: Uci.Drop): Validated[String, (Game, Drop)] = drop(uci.role, uci.pos)
  def apply(uci: Uci): Validated[String, (Game, MoveOrDrop)] =
    uci match
      case u: Uci.Move => apply(u) map { case (g, m) => g -> Left(m) }
      case u: Uci.Drop => apply(u) map { case (g, d) => g -> Right(d) }

  inline def isStandardInit = board.pieces == chess.variant.Standard.pieces

  inline def fullMoveNumber: FullMoveNumber = turns.fullMoveNumber

  def moveString = s"$fullMoveNumber${player.fold(".", "...")}"

  inline def withBoard(inline b: Board) = copy(situation = situation.copy(board = b))

  inline def updateBoard(inline f: Board => Board) = withBoard(f(board))

  inline def withPlayer(c: Color) = copy(situation = situation.copy(color = c))

  inline def withTurns(t: Ply) = copy(turns = t)

object Game:

  def apply(variant: chess.variant.Variant): Game =
    Game(Situation(Board init variant, White))

  def apply(board: Board): Game = apply(board, White)

  def apply(board: Board, color: Color): Game = new Game(Situation(board, color))

  def apply(variantOption: Option[chess.variant.Variant], fen: Option[Fen.Epd]): Game =
    val variant = variantOption | chess.variant.Standard
    val g       = apply(variant)
    fen
      .flatMap {
        format.Fen.readWithMoveNumber(variant, _)
      }
      .fold(g) { parsed =>
        g.copy(
          situation = Situation(
            board = parsed.situation.board withVariant g.board.variant withCrazyData {
              parsed.situation.board.crazyData orElse g.board.crazyData
            },
            color = parsed.situation.color
          ),
          turns = parsed.ply,
          startedAtTurn = parsed.ply
        )
      }
