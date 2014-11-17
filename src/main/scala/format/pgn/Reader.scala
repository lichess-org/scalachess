package chess
package format.pgn

import format.Forsyth

object Reader {

  def full(pgn: String, tags: List[Tag] = Nil): Valid[Replay] =
    fullWithSans(pgn, identity, tags)

  def moves(moveStrs: List[String], tags: List[Tag], trusted: Boolean): Valid[Replay] =
    movesWithSans(moveStrs, identity, tags, trusted)

  def fullWithSans(
    pgn: String,
    op: List[San] => List[San],
    tags: List[Tag] = Nil): Valid[Replay] = for {
    parsed ← Parser.full(pgn)
    game ← makeGame(parsed.tags ::: tags)
    replay ← makeReplay(game, op(parsed.sans), trusted = false)
  } yield replay

  def movesWithSans(
    moveStrs: List[String],
    op: List[San] => List[San],
    tags: List[Tag],
    trusted: Boolean): Valid[Replay] = for {
    moves ← Parser.moves(moveStrs)
    game ← makeGame(tags)
    replay ← makeReplay(game, op(moves), trusted)
  } yield replay

  private def makeReplay(game: Game, sans: List[San], trusted: Boolean) =
    sans.foldLeft[Valid[Replay]](Replay(game).success) {
      case (replayValid, san) => for {
        replay ← replayValid
        move ← san(replay.state.situation, trusted)
      } yield replay addMove move
    }

  private def makeGame(tags: List[Tag]): Valid[Game] =
    tags.foldLeft(success(Game(chess.Variant.default)): Valid[Game]) {
      case (vg, Tag(Tag.FEN, fen)) => for {
        g1 ← vg
        parsed ← (Forsyth <<< fen) toValid "Invalid fen " + fen
      } yield g1.copy(
        board = parsed.situation.board,
        player = parsed.situation.color,
        turns = parsed.turns)
      case (vg, Tag(Tag.Variant, name)) => vg map { g1 =>
        val variant = chess.Variant.byName(name) | chess.Variant.default
        g1 updateBoard (_ withVariant variant)
      }
      case (vg, _) => vg
    }
}
