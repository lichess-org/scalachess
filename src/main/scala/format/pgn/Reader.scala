package chess
package format.pgn

import format.Forsyth
import scalaz.Validation.FlatMap._

object Reader {

  def full(pgn: String, tags: List[Tag] = Nil): Valid[Replay] =
    fullWithSans(pgn, identity, tags)

  def moves(moveStrs: List[String], tags: List[Tag]): Valid[Replay] =
    movesWithSans(moveStrs, identity, tags)

  def fullWithSans(
    pgn: String,
    op: List[San] => List[San],
    tags: List[Tag] = Nil): Valid[Replay] = for {
    parsed ← Parser.full(pgn)
    game ← makeGame(parsed.tags ::: tags)
    replay ← makeReplay(game, op(parsed.sans))
  } yield replay

  def movesWithSans(
    moveStrs: List[String],
    op: List[San] => List[San],
    tags: List[Tag]): Valid[Replay] = for {
    moves ← Parser.moves(moveStrs, Parser.getVariantFromTags(tags))
    game ← makeGame(tags)
    replay ← makeReplay(game, op(moves))
  } yield replay

  private def makeReplay(game: Game, sans: List[San]) =
    sans.foldLeft[Valid[Replay]](Replay(game).success) {
      case (replayValid, san) => for {
        replay ← replayValid
        move ← san(replay.state.situation)
      } yield replay addMove move
    }

  private def makeGame(tags: List[Tag]): Valid[Game] =
    tags.foldLeft(success(Game(chess.variant.Variant.default)): Valid[Game]) {
      case (vg, Tag(Tag.FEN, fen)) => for {
        g1 ← vg
        parsed ← (Forsyth <<< fen) toValid "Invalid fen " + fen
      } yield g1.copy(
        board = parsed.situation.board,
        player = parsed.situation.color,
        turns = parsed.turns)
      case (vg, Tag(Tag.Variant, name)) => vg map { g1 =>
        val variant = chess.variant.Variant.byName(name) | chess.variant.Variant.default
        g1 updateBoard (_ withVariant variant)
      }
      case (vg, _) => vg
    }
}
