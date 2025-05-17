package chess
package format.pgn

import cats.syntax.all.*

object Reader:

  case class Result(replay: Replay, failure: Option[ErrorStr]):
    def valid: Either[ErrorStr, Replay] =
      failure.fold(replay.asRight)(_.asLeft)

  def mainline(pgn: PgnStr): Either[ErrorStr, Result] =
    Parser.mainline(pgn).map(ml => makeReplay(Game(ml.tags), Sans(ml.sans)))

  def moves(sans: Iterable[SanStr], tags: Tags): Either[ErrorStr, Result] =
    moves(sans, identity, tags)

  def moves(sans: Iterable[SanStr], game: Game): Either[ErrorStr, Result] =
    Parser
      .moves(sans)
      .map(moves => makeReplay(game, moves))

  def full(parsed: ParsedPgn, op: Sans => Sans): Result =
    makeReplay(Game(parsed.tags), op(Sans(parsed.mainline)))

  def moves(sans: Iterable[SanStr], op: Sans => Sans, tags: Tags): Either[ErrorStr, Result] =
    Parser
      .moves(sans)
      .map(moves => makeReplay(Game(tags), op(moves)))

  def makeReplay(game: Game, sans: Sans): Result =
    val (state, moves, error) = game.playReverse(sans.value)
    Result(Replay(game, moves, state), error)
