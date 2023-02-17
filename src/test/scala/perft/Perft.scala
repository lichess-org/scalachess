package chess
package perft

import chess.format.EpdFen
import chess.variant.Chess960
import chess.variant.Variant
import org.specs2.specification.core.*
import chess.variant.Crazyhouse
import MoveOrDrop.*

case class Perft(id: String, epd: EpdFen, cases: List[TestCase]):
  import Perft.*
  def calculate(variant: Variant): List[Result] =
    val game = Game(Option(variant), Option(epd))
    cases.map(c =>
      // printResult(game.divide(c.depth))
      Result(c.depth, game.perft(c.depth), c.result)
    )

  def withLimit(limit: Long): Perft =
    copy(cases = cases.filter(_.result < limit))

case class TestCase(depth: Int, result: Long)
case class Result(depth: Int, result: Long, expected: Long)

case class DivideResult(val move: MoveOrDrop, nodes: Long) {
  override def toString(): String =
    s"${move.toUci.uci} $nodes"
}

object Perft:

  lazy val threeCheckPerfts  = Perft.read("3check.perft")
  lazy val antichessPerfts   = Perft.read("antichess.perft")
  lazy val atomicPerfts      = Perft.read("atomic.perft")
  lazy val crazyhousePerfts  = Perft.read("crazyhouse.perft")
  lazy val hordePerfts       = Perft.read("horde.perft")
  lazy val racingkingsPerfts = Perft.read("racingkings.perft")
  lazy val randomPerfts      = Perft.read("random.perft")
  lazy val trickyPerfts      = Perft.read("tricky.perft")
  lazy val chess960          = Perft.read("chess960.perft")

  private def read(file: String): List[Perft] =
    import cats.implicits.toShow
    val str = io.Source.fromResource(file).mkString
    Parser.parse(str).fold(ex => throw RuntimeException(s"Parsing error: $file: ${ex.show}"), identity)

  def printResult(results: List[DivideResult]) =
    val builder = StringBuilder()
    var sum     = 0L
    results.foreach { r =>
      sum += r.nodes
      builder.append(r).append("\n")
    }
    builder.append("\n").append(sum)
    println(builder)

  extension (game: Game)

    private def apply(md: MoveOrDrop): Game = md match
      case m: Move => game.apply(m)
      case d: Drop => game.applyDrop(d)

    def divide(depth: Int): List[DivideResult] =
      if depth == 0 then Nil
      else if game.situation.perftEnd then Nil
      else
        game.perftMoves
          .map { move =>
            val nodes = game(move).perft(depth - 1)
            DivideResult(move, nodes)
          }
          .sortBy(_.move.toUci.uci)

    def perft(depth: Int): Long =
      if depth == 0 then 1L
      else if game.situation.perftEnd then 0L
      else
        val moves = game.perftMoves
        if (depth == 1) then moves.size.toLong
        else moves.map(game(_).perft(depth - 1)).sum

    private def perftMoves: List[MoveOrDrop] =
      if game.situation.board.variant == chess.variant.Crazyhouse
      then Crazyhouse.legalMoves(game.situation)
      else
        val legalMoves = game.situation.legalMoves
        if game.situation.board.variant.chess960 then legalMoves
        // if variant is not chess960 we need to deduplicated castlings moves
        // We filter out castling move that is Standard and king's dest is not in the rook position
        else legalMoves.filterNot(m => m.castle.exists(c => c.isStandard && m.dest != c.rook))

    private def crazyhousePerftMoves: List[MoveOrDrop] =
      Crazyhouse.legalMoves(game.situation)

  extension (s: Situation)
    // when calculate perft we don't do autoDraw
    def perftEnd = s.checkMate || s.staleMate || s.variantEnd || s.board.variant.specialDraw(s)
