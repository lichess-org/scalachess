package chess
package format.pgn

import cats.syntax.all.*

object Reader:

  case class Result(replay: Replay, failure: Option[ErrorStr]):
    def valid: Either[ErrorStr, Replay] =
      failure.fold(replay.asRight)(_.asLeft)

  def full(pgn: PgnStr, tags: Tags = Tags.empty): Either[ErrorStr, Result] =
    Parser.mainline(pgn).map(fullWithSans)

  def moves(sans: Iterable[SanStr], tags: Tags): Either[ErrorStr, Result] =
    movesWithSans(sans, identity, tags)

  def fullWithSans(parsed: ParsedMainline[San]): Result =
    makeReplay(makeGame(parsed.tags), Sans(parsed.sans))

  def fullWithSans(parsed: ParsedPgn, op: Sans => Sans): Result =
    makeReplay(makeGame(parsed.tags), op(Sans(parsed.mainline)))

  def movesWithSans(sans: Iterable[SanStr], op: Sans => Sans, tags: Tags): Either[ErrorStr, Result] =
    Parser
      .moves(sans)
      .map(moves => makeReplay(makeGame(tags), op(moves)))

  private def makeReplay(game: Game, sans: Sans): Result =
    sans.value.zipWithIndex
      .foldM(Replay(game)) { case (replay, (san, index)) =>
        san(replay.state.board).bimap(_ => (replay, makeError(game.ply + index, san)), replay.addMove(_))
      }
      .match
        case Left(replay, err) => Result(replay, err.some)
        case Right(replay)     => Result(replay, none)

  inline def makeError(currentPly: Ply, san: San): ErrorStr =
    val moveAt = currentPly.fullMoveNumber.value
    val move   = san.rawString.getOrElse(san.toString)
    ErrorStr(s"Cannot play $move at move $moveAt by ${currentPly.turn.name}")

  private def makeGame(tags: Tags) =
    val g = Game(variantOption = tags.variant, fen = tags.fen)
    g.copy(startedAtPly = g.ply, clock = tags.timeControl.flatMap(_.toClockConfig).map(Clock.apply))
