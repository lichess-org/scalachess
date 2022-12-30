package chess
package perft

import chess.format.EpdFen
import chess.variant.Chess960
import chess.variant.Variant
import org.specs2.specification.core.*

case class Perft1(id: String, epd: EpdFen, cases: List[TestCase])
case class Perft(id: String, epd: EpdFen, cases: List[TestCase], variant: Variant):
  def calculate(): List[Result] =
    val game = Game(Option(variant), Option(epd))
    cases.map(c => Result(c.depth, Perft.perft(game, c.depth), c.result))

case class TestCase(depth: Int, result: Long)
case class Result(depth: Int, result: Long, expected: Long)

object Perft:

  def read(file: String) =
    val str = io.Source.fromResource(file).mkString
    println(Parser.parse(str))

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

  // TODO the link doesn't exist anything
  // source: https://marcelk.net/rookie/nostalgia/v3/perft-random.epd
  val chess960 = List(
    Perft(
      "gentest-1364",
      EpdFen("8/4k3/2r1Br2/Pp4Q1/3N4/1p5K/1b6/8 b - -"),
      List(TestCase(3, 18802)),
      Chess960
    ),
    Perft(
      "gentest-2309",
      EpdFen("7k/8/1p2p2n/Pp2P2p/7P/Q1p1P3/3NK3/4N2B b - -"),
      List(TestCase(3, 4760)),
      Chess960
    ),
    Perft(
      "gentest-2698",
      EpdFen("8/8/4R3/1k6/8/5n2/Kb6/8 w - -"),
      List(TestCase(3, 5444)),
      Chess960
    ),
    Perft(
      "gentest-3283",
      EpdFen("r1bqkb1r/p1pp1ppp/1p3n2/6B1/Pn1p2P1/7B/RPP1PP1P/1N1QK1NR w Kkq -"),
      List(TestCase(3, 26302)),
      Chess960
    ),
    Perft(
      "gentest-4253",
      EpdFen("2Bk4/2N4N/1Q6/P2pb1P1/1R5p/2r1p2P/4K1R1/8 b - -"),
      List(TestCase(3, 17116)),
      Chess960
    ),
    Perft(
      "gentest-4309",
      EpdFen("1nb1qbn1/4rk1r/ppNp3p/5ppP/P5P1/1P3P2/2PPP1RN/1R1QKB2 b - -"),
      List(TestCase(3, 20587)),
      Chess960
    ),
    Perft(
      "gentest-5008",
      EpdFen("5nk1/7p/r1p4b/p5PP/K4p2/3R4/5B2/8 w - -"),
      List(TestCase(3, 8214)),
      Chess960
    ),
    Perft(
      "gentest-5569",
      EpdFen("r3k1n1/p2p2p1/1p4N1/2r2pq1/2b2PPp/b1p5/P3PB1P/RQ1K1B1R b q -"),
      List(TestCase(3, 54944)),
      Chess960
    ),
    Perft(
      "gentest-6195",
      EpdFen("Bnbqk2r/2pppp2/pp4p1/7p/P2b2Q1/2P1P3/1P1P1PPP/RNB2KNR w k -"),
      List(TestCase(3, 41002)),
      Chess960
    ),
    Perft(
      "gentest-6689",
      EpdFen("8/k5b1/7N/2b5/8/1K3N2/8/4R3 w - -"),
      List(TestCase(3, 17996)),
      Chess960
    ),
    // source: http://www.talkchess.com/forum/viewtopic.php?t=55274
    Perft(
      "x-fen 00",
      EpdFen("r1k1r2q/p1ppp1pp/8/8/8/8/P1PPP1PP/R1K1R2Q w KQkq - 0 1"),
      List(TestCase(3, 12333)),
      Chess960
    ),
    Perft(
      "x-fen 01",
      EpdFen("r1k2r1q/p1ppp1pp/8/8/8/8/P1PPP1PP/R1K2R1Q w KQkq - 0 1"),
      List(TestCase(3, 20218)),
      Chess960
    ),
    Perft(
      "x-fen 02",
      EpdFen("8/8/8/4B2b/6nN/8/5P2/2R1K2k w Q - 0 1"),
      List(TestCase(4, 118388)),
      Chess960
    ),
    Perft(
      "x-fen 03",
      EpdFen("2r5/8/8/8/8/8/6PP/k2KR3 w K - 0 1"),
      List(TestCase(4, 57700)),
      Chess960
    ),
    Perft(
      "x-fen 04",
      EpdFen("4r3/3k4/8/8/8/8/6PP/qR1K1R2 w KQ - 0 1"),
      List(TestCase(3, 12858)),
      Chess960
    )
  )

  val tricky: List[Perft] = List(
    // source: https://chessprogramming.wikispaces.com/Perft+Results
    Perft(
      "kiwipete",
      EpdFen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"),
      List(TestCase(3, 97862)),
      Chess960
    ),
    Perft(
      "position 4 mirrored",
      EpdFen("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1"),
      List(TestCase(3, 9467)),
      Chess960
    ),
    // https://github.com/ornicar/lila/issues/4625
    Perft(
      "h-side rook blocks a-side castling",
      EpdFen("4rrk1/pbbp2p1/1ppnp3/3n1pqp/3N1PQP/1PPNP3/PBBP2P1/4RRK1 w Ff - 0 1"),
      List(TestCase(3, 71908)),
      Chess960
    )
  )
