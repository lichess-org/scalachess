package chess

import chess.format.Fen
import chess.format.{ pgn, Uci }
import chess.format.pgn.SanStr
import chess.format.{ Fen, Uci, pgn }

case class Game(
    situation: Situation,
    sans: Vector[SanStr] = Vector(),
    clock: Option[Clock] = None,
    ply: Ply = Ply.initial, // plies
    startedAtPly: Ply = Ply.initial
):

  export situation.{ board, color as player }
  export situation.board.history.halfMoveClock

  def apply(
      orig: Square,
      dest: Square,
      promotion: Option[PromotableRole] = None,
      metrics: MoveMetrics = MoveMetrics.empty
  ): Either[ErrorStr, (Game, Move)] =
    moveWithCompensated(orig, dest, promotion, metrics).map((game, move) => (game.value, move))

  def moveWithCompensated(
      orig: Square,
      dest: Square,
      promotion: Option[PromotableRole] = None,
      metrics: MoveMetrics = MoveMetrics.empty
  ): Either[ErrorStr, (Clock.WithCompensatedLag[Game], Move)] =
    situation
      .move(orig, dest, promotion)
      .map(_.normalizeCastle.withMetrics(metrics))
      .map(move => applyWithCompensated(move) -> move)

  def apply(move: Move): Game = applyWithCompensated(move).value

  def applyWithCompensated(move: Move): Clock.WithCompensatedLag[Game] =
    val newSituation = move.situationAfter
    val newClock     = applyClock(move.metrics, newSituation.status.isEmpty)

    Clock.WithCompensatedLag(
      copy(
        situation = newSituation,
        ply = ply + 1,
        sans = sans :+ move.san,
        clock = newClock.map(_.value)
      ),
      newClock.flatMap(_.compensated)
    )

  def drop(
      role: Role,
      square: Square,
      metrics: MoveMetrics = MoveMetrics.empty
  ): Either[ErrorStr, (Game, Drop)] =
    situation.drop(role, square).map(_.withMetrics(metrics)).map(drop => applyDrop(drop) -> drop)

  def applyDrop(drop: Drop): Game =
    val newSituation = drop.situationAfter
    copy(
      situation = newSituation,
      ply = ply + 1,
      sans = sans :+ drop.san,
      clock = applyClock(drop.metrics, newSituation.status.isEmpty).map(_.value)
    )

  private def applyClock(
      metrics: MoveMetrics,
      gameActive: => Boolean
  ): Option[Clock.WithCompensatedLag[Clock]] =
    clock.map: prev =>
      val c1 = metrics.frameLag.fold(prev)(prev.withFrameLag)
      val c2 = c1.step(metrics, gameActive)
      if ply - startedAtPly == Ply(1) then c2.map(_.start) else c2

  def apply(uci: Uci.Move): Either[ErrorStr, (Game, Move)] = apply(uci.orig, uci.dest, uci.promotion)
  def apply(uci: Uci.Drop): Either[ErrorStr, (Game, Drop)] = drop(uci.role, uci.square)
  def apply(uci: Uci): Either[ErrorStr, (Game, MoveOrDrop)] =
    uci match
      case uci: Uci.Move => apply(uci)
      case uci: Uci.Drop => apply(uci)

  inline def fullMoveNumber: FullMoveNumber = ply.fullMoveNumber

  inline def withBoard(inline b: Board) = copy(situation = situation.copy(board = b))

  inline def updateBoard(inline f: Board => Board) = withBoard(f(board))

  inline def withPlayer(c: Color) = copy(situation = situation.copy(color = c))

  inline def withTurns(t: Ply) = copy(ply = t)

object Game:

  def apply(variant: chess.variant.Variant): Game =
    Game(Situation(Board.init(variant), White))

  def apply(board: Board): Game = apply(board, White)

  def apply(board: Board, color: Color): Game = Game(Situation(board, color))

  def apply(variantOption: Option[chess.variant.Variant], fen: Option[Fen.Epd]): Game =
    val variant = variantOption | chess.variant.Standard
    val g       = apply(variant)
    fen
      .flatMap:
        format.Fen.readWithMoveNumber(variant, _)
      .fold(g): parsed =>
        g.copy(
          situation = Situation(
            board = parsed.situation.board.withVariant(g.board.variant).withCrazyData {
              parsed.situation.board.crazyData.orElse(g.board.crazyData)
            },
            color = parsed.situation.color
          ),
          ply = parsed.ply,
          startedAtPly = parsed.ply
        )
