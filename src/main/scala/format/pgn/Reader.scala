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
    replay ← op(parsed.sans).foldLeft(Replay(game).success: Valid[Replay]) {
      case (replayValid, san) ⇒ for {
        replay ← replayValid
        move ← san(replay.game)
      } yield new Replay(
        game = replay game move,
        moves = move :: replay.moves)
    }
  } yield replay

  def makeGame(tags: List[Tag]): Valid[Game] =
    tags.foldLeft(success(Game(chess.Variant.default)): Valid[Game]) {
      case (vg, Fen(fen)) ⇒ for {
        g1 ← vg
        parsed ← (Forsyth << fen) toValid "Invalid fen " + fen
      } yield g1.copy(board = parsed.board, player = parsed.color)
      case (vg, Variant(name)) ⇒ for {
        g1 ← vg
        variant ← chess.Variant(name) toValid "Invalid variant " + name
      } yield g1 updateBoard (_ withVariant variant)
      case (vg, _) ⇒ vg
    }
}
