package chess
package format.pgn

import cats.syntax.all.*

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
      .mainline(pgn)
      .map(parsed => makeReplay(makeGame(parsed.tags ++ tags), op(Sans(parsed.sans))))

  def fullWithSans(parsed: ParsedPgn, op: Sans => Sans): Result =
    makeReplay(makeGame(parsed.tags), op(Sans(parsed.mainline)))

  def movesWithSans(sans: Iterable[SanStr], op: Sans => Sans, tags: Tags): Either[ErrorStr, Result] =
    Parser
      .moves(sans)
      .map(moves => makeReplay(makeGame(tags), op(moves)))

  private def makeReplay(game: Game, sans: Sans): Result =
    sans.value.zipWithIndex
      .foldM(Replay(game)) { case (replay, (san, index)) =>
        san(replay.state.situation).bimap(_ => (replay, makeError(index, game.ply, san)), replay.addMove(_))
      }
      .match
        case Left(replay, err) => Result.Incomplete(replay, err)
        case Right(replay)     => Result.Complete(replay)

  inline def makeError(index: Int, startedPly: Ply, san: San): ErrorStr =
    val ply    = (startedPly + index).next
    val moveAt = ply.fullMoveNumber.value
    val move   = san.rawString.getOrElse(san.toString)
    ErrorStr(s"Cannot play $move at move $moveAt, ply $ply")

  private def makeGame(tags: Tags) =
    val g = Game(variantOption = tags.variant, fen = tags.fen)
    g.copy(startedAtPly = g.ply, clock = tags.timeControl.flatMap(_.toClockConfig).map(Clock.apply))
