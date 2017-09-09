package chess
package format.pgn

import scalaz.Validation.FlatMap._

object Reader {

  def full(pgn: String, tags: List[Tag] = Nil): Valid[Replay] =
    fullWithSans(pgn, identity, tags)

  def moves(moveStrs: Traversable[String], tags: List[Tag]): Valid[Replay] =
    movesWithSans(moveStrs, identity, tags)

  def fullWithSans(
    pgn: String,
    op: List[San] => List[San],
    tags: List[Tag] = Nil
  ): Valid[Replay] = for {
    parsed ← Parser.full(pgn)
    game = makeGame(parsed.tags ::: tags)
    replay ← makeReplay(game, op(parsed.sans))
  } yield replay

  def fullWithSans(parsed: ParsedPgn, op: List[San] => List[San]): Valid[Replay] =
    makeReplay(makeGame(parsed.tags), op(parsed.sans))

  def movesWithSans(
    moveStrs: Traversable[String],
    op: List[San] => List[San],
    tags: List[Tag]
  ): Valid[Replay] = for {
    moves ← Parser.moves(moveStrs, Parser.getVariantFromTags(tags))
    game = makeGame(tags)
    replay ← makeReplay(game, op(moves))
  } yield replay

  private def makeReplay(game: Game, sans: List[San]) =
    sans.foldLeft[Valid[Replay]](Replay(game).success) {
      case (replayValid, san) => for {
        replay ← replayValid
        move ← san(replay.state.situation)
      } yield replay addMove move
    }

  private def makeGame(tags: List[Tag]) = {
    def tag(which: TagType): Option[String] = tags find (_.name == which) map (_.value)
    val g = Game(
      variantOption = tag(Tag.Variant) flatMap chess.variant.Variant.byName,
      fen = tag(Tag.FEN)
    )
    g.copy(
      startedAtTurn = g.turns,
      clock = tag(Tag.TimeControl) flatMap Clock.readPgnConfig map Clock.apply
    )
  }
}
