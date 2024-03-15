package chess
package format.pgn

import cats.syntax.all.*
import MoveOrDrop.*

object PgnHelper:
  case class Context(sit: Situation, ply: Ply)

  extension (d: PgnNodeData)
    def toMove(context: Context): Option[(Situation, Move)] =
      d.san(context.sit)
        .toOption
        .map(x =>
          (
            x.situationAfter,
            Move(
              ply = context.ply,
              san = x.toSanStr,
              comments = d.comments,
              glyphs = d.glyphs,
              opening = None,
              result = None,
              secondsLeft = None,
              variationComments = d.variationComments
            )
          )
        )

  extension (tree: ParsedPgnTree)
    def toPgn(game: Game): Option[PgnTree] =
      tree.mapAccumlOption_(Context(game.situation, game.ply + 1)): (ctx, d) =>
        d.toMove(ctx) match
          case Some((sit, m)) => (Context(sit, ctx.ply.next), m.some)
          case None           => (ctx, None)

  extension (pgn: ParsedPgn)
    def toPgn: Pgn =
      val game = makeGame(pgn.tags)
      Pgn(pgn.tags, pgn.initialPosition, pgn.tree.flatMap(_.toPgn(game)))

    def cleanTags: ParsedPgn =
      pgn.copy(tags = Tags.empty)

  extension (pgn: PgnStr)
    def cleanup = PgnStr:
      pgn.value.replace("\r", "").replace("\n", " ").trim

  private def makeGame(tags: Tags) =
    val g = Game(
      variantOption = tags(_.Variant).flatMap(chess.variant.Variant.byName),
      fen = tags.fen
    )
    g.copy(
      startedAtPly = g.ply,
      clock = tags.clockConfig.map(Clock.apply)
    )
