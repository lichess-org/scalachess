package chess
package format.pgn

import cats.syntax.all.*
import util.chaining.scalaUtilChainingOps

object Reader:

  sealed trait Result:
    def valid: Either[ErrorStr, Replay]

  object Result:
    case class Complete(replay: Replay) extends Result:
      def valid = replay.asRight
    case class Incomplete(replay: Replay, failure: ErrorStr) extends Result:
      def valid = failure.asLeft

  def full(pgn: PgnStr, tags: Tags = Tags.empty): Either[ErrorStr, Result] =
    fullWithSans(pgn, identity, tags)

  def moves(sans: Iterable[SanStr], tags: Tags): Either[ErrorStr, Result] =
    movesWithSans(sans, identity, tags)

  def fullWithSans(pgn: PgnStr, op: Sans => Sans, tags: Tags = Tags.empty): Either[ErrorStr, Result] =
    Parser
      .full(pgn)
      .map: parsed =>
        makeReplay(makeGame(parsed.tags ++ tags), op(Sans(parsed.mainline)))

  def fullWithSans(parsed: ParsedPgn, op: Sans => Sans): Result =
    makeReplay(makeGame(parsed.tags), op(Sans(parsed.mainline)))

  def movesWithSans(sans: Iterable[SanStr], op: Sans => Sans, tags: Tags): Either[ErrorStr, Result] =
    Parser
      .moves(sans)
      .map: moves =>
        makeReplay(makeGame(tags), op(moves))

  // TODO
  private def makeReplay(game: Game, sans: Sans): Result =
    sans.value.foldLeft[Result](Result.Complete(Replay(game))):
      case (Result.Complete(replay), san) =>
        san(replay.state.situation).fold(
          err => Result.Incomplete(replay, err),
          move => Result.Complete(replay addMove move)
        )
      case (r: Result.Incomplete, _) => r

  private def makeGame(tags: Tags) =
    Game(variantOption = tags.variant, fen = tags.fen).pipe(self =>
      self.copy(startedAtPly = self.ply, clock = tags.clockConfig.map(Clock.apply))
    )
