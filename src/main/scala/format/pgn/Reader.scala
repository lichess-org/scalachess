package chess
package format.pgn

import format.Forsyth

object Reader {

  def apply(pgn: String, tags: List[Tag] = Nil): Valid[Replay] =
    withSans(pgn, identity, tags)

  def withSans(
    pgn: String,
    op: List[San] ⇒ List[San],
    tags: List[Tag] = Nil): Valid[Replay] = for {
    parsed ← Parser(pgn)
    game ← makeGame(parsed.tags ::: tags)
    replay ← op(parsed.sans).foldLeft[Valid[Replay]](Replay(game).success) {
      case (replayValid, san) ⇒ for {
        replay ← replayValid
        move ← san(replay.state)
      } yield replay addMove move
    }
  } yield replay

  def makeGame(tags: List[Tag]): Valid[Game] =
    tags.foldLeft(success(Game(chess.Variant.default)): Valid[Game]) {
      case (vg, Tag(Tag.FEN, fen)) ⇒ for {
        g1 ← vg
        parsed ← (Forsyth <<< fen) toValid "Invalid fen " + fen
      } yield g1.copy(
        board = parsed.situation.board,
        player = parsed.situation.color,
        turns = parsed.turns)
      case (vg, Tag(Tag.Variant, name)) ⇒ vg map { g1 =>
        val variant = chess.Variant(name.toLowerCase) | chess.Variant.default
        g1 updateBoard (_ withVariant variant)
      case (vg, _) ⇒ vg
    }
}
