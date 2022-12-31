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

  def withLimit(limit: Long): Perft =
    copy(cases = cases.filter(_.result < limit))

case class TestCase(depth: Int, result: Long)
case class Result(depth: Int, result: Long, expected: Long)

object Perft:

  lazy val threeCheckPerfts  = Perft.read("3check.perft")
  lazy val antichessPerfts   = Perft.read("antichess.perft")
  lazy val atomicPerfts       = Perft.read("atomic.perft")
  lazy val crazyhousePerfts  = Perft.read("crazyhouse.perft")
  lazy val hordePerfts       = Perft.read("horde.perft")
  lazy val racingkingsPerfts = Perft.read("racingkings.perft")
  lazy val randomPerfts      = Perft.read("random.perft")
  lazy val trickyPerfts      = Perft.read("tricky.perft")

  private def read(file: String): List[Perft] =
    val str = io.Source.fromResource(file).mkString
    Parser.parse(str).getOrElse(throw RuntimeException(s"Parse perft file failed: $file"))

  private def perft(game: Game, depth: Int): Long =
    if depth == 0 then 1L
    else
      val allMoves = game.situation.allTrustedMoves
      // if variant is not chess960 we need to deduplicated castlings moves
      val moves =
        if !game.situation.board.variant.chess960 then
          allMoves.filterNot(m => m.castles && (m.dest == File.A || m.dest == File.H))
        else allMoves
      if (depth == 1) then moves.size.toLong
      else moves.map(move => perft(game.apply(move), depth - 1)).sum
