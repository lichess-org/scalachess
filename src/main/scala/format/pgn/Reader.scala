package chess
package format.pgn

import scalaz.Validation.FlatMap._

object Reader {

  def full(pgn: String, tags: Tags = Tags.empty): Valid[Replay] =
    fullWithSans(pgn, identity, tags)

  def moves(moveStrs: Traversable[String], tags: Tags): Valid[Replay] =
    movesWithSans(moveStrs, identity, tags)

  def fullWithSans(
    pgn: String,
    op: Sans => Sans,
    tags: Tags = Tags.empty
  ): Valid[Replay] = for {
    parsed ← Parser.full(pgn)
    game = makeGame(parsed.tags ++ tags)
    replay ← makeReplay(game, op(parsed.sans))
  } yield replay

  def fullWithSans(parsed: ParsedPgn, op: Sans => Sans): Valid[Replay] =
    makeReplay(makeGame(parsed.tags), op(parsed.sans))

  def movesWithSans(
    moveStrs: Traversable[String],
    op: Sans => Sans,
    tags: Tags
  ): Valid[Replay] = for {
    moves ← Parser.moves(moveStrs, tags.variant | variant.Variant.default)
    game = makeGame(tags)
    replay ← makeReplay(game, op(moves))
  } yield replay

  private def makeReplay(game: Game, sans: Sans) =
    sans.value.foldLeft[Valid[Replay]](Replay(game).success) {
      case (replayValid, san) => for {
        replay ← replayValid
        move ← san(replay.state.situation)
      } yield replay addMove move
    }

  private def makeGame(tags: Tags) = {
    val g = Game(
      variantOption = tags(_.Variant) flatMap chess.variant.Variant.byName,
      fen = tags(_.FEN)
    )
    g.copy(
      startedAtTurn = g.turns,
      clock = tags.clockConfig map Clock.apply
    )
  }
}
