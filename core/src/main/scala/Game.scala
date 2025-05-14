package chess

import chess.format.pgn.{ SanStr, Tags }
import chess.format.{ Fen, Uci }

case class Game(
    position: Position,
    sans: Vector[SanStr] = Vector(),
    clock: Option[Clock] = None,
    ply: Ply = Ply.initial, // plies
    startedAtPly: Ply = Ply.initial
):

  export position.{ color as player, variant, history }
  export position.history.halfMoveClock

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
    position
      .move(orig, dest, promotion)
      .map(_.normalizeCastle.withMetrics(metrics))
      .map(move => applyWithCompensated(move) -> move)

  def drop(
      role: Role,
      square: Square,
      metrics: MoveMetrics = MoveMetrics.empty
  ): Either[ErrorStr, (Game, Drop)] =
    position.drop(role, square).map(_.withMetrics(metrics)).map(drop => applyDrop(drop) -> drop)

  def apply(move: Move): Game =
    applyWithCompensated(move).value

  def applyWithCompensated(move: Move): Clock.WithCompensatedLag[Game] =
    val newPosition = move.after
    val newClock    = applyClock(move.metrics, newPosition.status.isEmpty)

    Clock.WithCompensatedLag(
      copy(
        position = newPosition,
        ply = ply + 1,
        sans = sans :+ move.toSanStr,
        clock = newClock.map(_.value)
      ),
      newClock.flatMap(_.compensated)
    )

  def applyDrop(drop: Drop): Game =
    val newPosition = drop.after
    copy(
      position = newPosition,
      ply = ply + 1,
      sans = sans :+ drop.toSanStr,
      clock = applyClock(drop.metrics, newPosition.status.isEmpty).map(_.value)
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

  inline def withPosition(inline p: Position): Game = copy(position = p)

  inline def updatePosition(inline f: Position => Position): Game = withPosition(f(position))

  inline def withPlayer(c: Color): Game = copy(position = position.copy(color = c))

  inline def withTurns(t: Ply): Game = copy(ply = t)

object Game:

  def apply(variant: chess.variant.Variant): Game =
    Game(Position.init(variant, White))

  def apply(variantOption: Option[chess.variant.Variant], fen: Option[Fen.Full]): Game =
    val variant = variantOption | chess.variant.Standard
    fen
      .flatMap(format.Fen.readWithMoveNumber(variant, _))
      .fold(Game(variant))(_.toGame)

  def apply(tags: Tags): Game =
    val g = Game(variantOption = tags.variant, fen = tags.fen)
    g.copy(startedAtPly = g.ply, clock = tags.timeControl.flatMap(_.toClockConfig).map(Clock.apply))

  given CanPlay[Game]:
    extension (game: Game)
      def apply[M <: Moveable](move: M): Either[ErrorStr, (Game, MoveOrDrop)] =
        move(game.position).map(md => md.applyGame(game) -> md)

      inline def position: Position = game.position
