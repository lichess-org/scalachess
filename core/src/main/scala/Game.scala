package chess

import chess.format.pgn.SanStr
import chess.format.{ Fen, Uci, pgn }

case class Game(
    board: Board,
    sans: Vector[SanStr] = Vector(),
    clock: Option[Clock] = None,
    ply: Ply = Ply.initial, // plies
    startedAtPly: Ply = Ply.initial
):

  export board.{ color as player, variant, history }
  export board.history.halfMoveClock

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
    board
      .move(orig, dest, promotion)
      .map(_.normalizeCastle.withMetrics(metrics))
      .map(move => applyWithCompensated(move) -> move)

  def apply(move: Move): Game = applyWithCompensated(move).value

  def applyWithCompensated(move: Move): Clock.WithCompensatedLag[Game] =
    val newBoard = move.boardAfter
    val newClock = applyClock(move.metrics, newBoard.status.isEmpty)

    Clock.WithCompensatedLag(
      copy(
        board = newBoard,
        ply = ply + 1,
        sans = sans :+ move.toSanStr,
        clock = newClock.map(_.value)
      ),
      newClock.flatMap(_.compensated)
    )

  def drop(
      role: Role,
      square: Square,
      metrics: MoveMetrics = MoveMetrics.empty
  ): Either[ErrorStr, (Game, Drop)] =
    board.drop(role, square).map(_.withMetrics(metrics)).map(drop => applyDrop(drop) -> drop)

  def applyDrop(drop: Drop): Game =
    val newBoard = drop.boardAfter
    copy(
      board = newBoard,
      ply = ply + 1,
      sans = sans :+ drop.toSanStr,
      clock = applyClock(drop.metrics, newBoard.status.isEmpty).map(_.value)
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

  inline def withBoard(inline b: Board): Game = copy(board = b)

  inline def updateBoard(inline f: Board => Board): Game = withBoard(f(board))

  inline def withPlayer(c: Color): Game = copy(board = board.copy(color = c))

  inline def withTurns(t: Ply): Game = copy(ply = t)

object Game:

  def apply(variant: chess.variant.Variant): Game =
    Game(Board.init(variant, White))

  def apply(variantOption: Option[chess.variant.Variant], fen: Option[Fen.Full]): Game =
    val variant = variantOption | chess.variant.Standard
    val g: Game = apply(variant)
    fen
      .flatMap(format.Fen.readWithMoveNumber(variant, _))
      .fold(g): parsed =>
        g.copy(
          board = parsed.board
            .withVariant(g.variant)
            .withCrazyData(parsed.board.crazyData.orElse(g.board.crazyData))
            .withColor(parsed.board.color),
          ply = parsed.ply,
          startedAtPly = parsed.ply
        )
