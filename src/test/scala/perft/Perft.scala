package chess
package perft

import chess.format.EpdFen
import chess.variant.Chess960
import chess.variant.Variant
import org.specs2.specification.core.*

case class Perft(id: String, epd: EpdFen, cases: List[TestCase]):
  def calculate(variant: Variant): List[Result] =
    val game = Game(Option(variant), Option(epd))
    cases.map(c => Result(c.depth, Perft.perft(game, c.depth), c.result))

case class TestCase(depth: Int, result: Long)
case class Result(depth: Int, result: Long, expected: Long)

object Perft:

  def read(file: String): List[Perft] =
    val str = io.Source.fromResource(file).mkString
    Parser.parse(str).getOrElse(throw RuntimeException(s"Parse perft file failed: $file"))

  private def perft(game: Game, depth: Int): Long =
    if (depth > 0)
      (game.situation.moves.values.toList.flatten: List[Move]).foldLeft(0L)((p, move) =>
        if (move.piece.role == Pawn && (move.dest.rank == Rank.First || move.dest.rank == Rank.Eighth))
          p + List(Queen, Rook, Bishop, Knight)
            .flatMap(move.withPromotion)
            .map(move => perft(game(move), depth - 1))
            .sum
        else
          p + perft(game.apply(move), depth - 1)
      )
    else 1L
