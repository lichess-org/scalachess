package chess
package format.pgn

import cats.syntax.all.*
import monocle.syntax.all.*
import snapshot4s.generated.snapshotConfig
import snapshot4s.munit.SnapshotAssertions

import scala.language.implicitConversions

class PgnRenderTest extends ChessTest with SnapshotAssertions:

  import Fixtures.*

  given Conversion[String, SanStr] = SanStr(_)
  given Conversion[String, Comment] = Comment(_)

  extension (pgn: ParsedPgn)
    def cleanTags: ParsedPgn =
      pgn.copy(tags = Tags.empty)

    def cleanRawString: ParsedPgn =
      pgn.copy(tree = pgn.tree.map(removeRawString))

  def removeRawString(san: San): San =
    san match
      case std: Std => std.copy(rawString = None)
      case drop: Drop => drop.copy(rawString = None)
      case castle: Castle => castle.copy(rawString = None)

  def removeRawString(node: Node[PgnNodeData]): Node[PgnNodeData] =
    node
      .focus(_.value.san)
      .modify(removeRawString)
      .map(_.focus(_.san).modify(removeRawString))

  lazy val pgns = List(
    pgn2,
    recentChessCom,
    chessComCrazyhouse,
    fromChessProgrammingWiki,
    fromPosProdCloseChess,
    noTagButResult,
    inlineTags,
    whiteResignsInMoves,
    whiteResignsInTags,
    stLouisFischerandom,
    inlineComments,
    fromChessgames,
    chessgamesWeirdComments,
    fromCrafty,
    fromTcecWithEngineOutput
  )

  test("pgn round trip tests compare ParsedPgn"):
    (pgns ++ wcc2023).foreach: x =>
      val pgn = Parser.full(x).get
      val output = Parser.full(pgn.toPgn.render).get
      assertEquals(output.cleanTags.cleanRawString, pgn.cleanTags.cleanRawString)

  test("pgn round trip tests compare Pgn"):
    List(pgn1, pgn3, pgn4).foreach: x =>
      val result = Parser.full(x).get.toPgn.render
      assertEquals(result.value, x)

  test("pgn round trip tests compare output"):
    val result = pgns.map(Parser.full(_).get.toPgn.render).mkString("\n\n")
    assertFileSnapshot(result, "pgn-render/pgns.txt")

  test("wcc2023 study round trip tests"):
    val result = wcc2023.map(Parser.full(_).get.toPgn.render).mkString("\n\n")
    assertFileSnapshot(result, "pgn-render/wcc2023.txt")

  val pgn1 = """
[Result "*"]

{ Root comment }
1. e4! $16 $40 $32 (1. d4?? d5 $146 { d5 is a good move }) (1. c4 { and }) (1. f4 { better }) 1... e6?! { e6 is a naughty move } *
  """.trim

  val pgn2 = """
[Event "WCh"]
[Site "Bonn GER"]
[Date "2008.10.14"]
[Round "1"]
[White "Kramnik,V"]
[Black "Anand,V"]
[Result "1/2-1/2"]
[WhiteElo "2772"]
[BlackElo "2783"]
[ECO "D14"]
[Annotator "IM Malcolm Pein"]
[EventDate "2008.10.14"]

{ It wasn't a riveting start but you don't get many risks taken in game one
when the score is still level. Kramnik asked a question, Anand answered
confidently }
1. d4 d5 2. c4 c6 3. Nc3 Nf6 4. cxd5 { The Exchange Slav, the sure way to
play with zero losing chances so an ideal choice for game one } 4... cxd5
5. Bf4 Nc6 6. e3 Bf5 7. Nf3 e6 { Black cannot continue symmetrically for
too long of course but this is the most solid choice } 8. Qb3 Bb4 9. Bb5
O-O { Black breaks the symmetry but this is still the main line of chess
opening theory } 10. Bxc6 (10. O-O Bxc3 11. Bxc6 Bxb2 12. Bxb7 Bxa1) 1/2-1/2
  """

  val pgn3 =
    """
[Result "1-0"]

1. e4 { [%clk 0:10:00] } 1... c5 { [%clk 0:10:00] } 2. Nf3 { [%clk 0:09:57] } 2... Nc6 { [%clk 0:09:58] } 3. Bb5 { [%clk 0:09:55] } { B30 Sicilian Defense: Nyezhmetdinov-Rossolimo Attack } 3... Qb6 { [%clk 0:09:56] } 4. Nc3 { [%clk 0:09:47] } 4... Nd4 { [%clk 0:09:54] } 5. Bc4 { [%clk 0:09:38] } 5... e6 { [%clk 0:09:52] } 6. O-O { [%clk 0:09:30] } 6... a6 { [%clk 0:09:50] } 7. d3 { [%clk 0:09:26] } 7... d6 { [%clk 0:09:46] } 8. Re1 { [%clk 0:09:13] } 8... Nf6 { [%clk 0:09:37] } 9. Rb1 { [%clk 0:08:43] } 9... Be7 { [%clk 0:09:13] } 10. Be3 { [%clk 0:08:33] } 10... Nxf3+ { [%clk 0:09:01] } 11. Qxf3 { [%clk 0:08:31] } 11... Qc7 { [%clk 0:08:58] } 12. a4 { [%clk 0:08:18] } 12... O-O { [%clk 0:08:51] } 13. Qg3 { [%clk 0:07:56] } 13... Kh8 { [%clk 0:08:47] } 14. f4 { [%clk 0:07:48] } 14... Qd8 { [%clk 0:08:28] } 15. e5 { [%clk 0:07:33] } 15... Nd7 { [%clk 0:08:12] } 16. exd6 { [%clk 0:07:12] } 16... Bxd6 { [%clk 0:08:08] } 17. Ne4 { [%clk 0:07:11] } 17... Be7 { [%clk 0:08:06] } 18. Qf2 { [%clk 0:05:35] } 18... Qc7 { [%clk 0:07:12] } 19. Ra1 { [%clk 0:03:29] } 19... a5 { [%clk 0:07:06] } 20. Bb5 { [%clk 0:03:02] } 20... b6 { [%clk 0:07:00] } 21. Qg3 { [%clk 0:02:46] } 21... Bb7 { [%clk 0:06:47] } 22. Bd2 { [%clk 0:02:42] } 22... Nf6 { [%clk 0:06:27] } 23. Ng5 { [%clk 0:02:37] } 23... h6 { [%clk 0:06:16] } 24. Qh3 { [%clk 0:02:34] } 24... Nd5 { [%clk 0:05:37] } 25. Rf1 { [%clk 0:02:24] } 25... Kg8 { [%clk 0:04:45] } 26. Ne4 { [%clk 0:02:20] } 26... f5 { [%clk 0:04:33] } 27. Ng3 { [%clk 0:02:15] } 27... Bf6 { [%clk 0:04:13] } 28. c3 { [%clk 0:02:14] } 28... Rad8 { [%clk 0:03:50] } 29. Rae1 { [%clk 0:02:11] } 29... Qf7 { [%clk 0:03:47] } 30. Nh5 { [%clk 0:02:09] } 30... Kh7 { [%clk 0:03:08] } 31. Nxf6+ { [%clk 0:01:57] } 31... Qxf6 { [%clk 0:03:07] } 32. Re5 { [%clk 0:01:50] } 32... Bc8 { [%clk 0:02:41] } 33. Rfe1 { [%clk 0:01:47] } 33... Nc7 { [%clk 0:02:34] } 34. Bc4 { [%clk 0:01:45] } 34... Rde8 { [%clk 0:02:31] } 35. Qf3 { [%clk 0:01:40] } 35... Re7 { [%clk 0:02:15] } 36. Be3 { [%clk 0:01:37] } 36... Rfe8 { [%clk 0:01:55] } 37. Bf2 { [%clk 0:01:31] } 37... Rd8 { [%clk 0:01:19] } 38. d4 { [%clk 0:01:29] } 38... Nd5 { [%clk 0:00:53] } 39. dxc5 { [%clk 0:01:26] } 39... bxc5 { [%clk 0:00:52] } 40. Bb5 { [%clk 0:01:08] } 40... Bb7 { [%clk 0:00:47] } 41. Qg3 { [%clk 0:01:02] } 41... Nc7 { [%clk 0:00:37] } 42. Bc4 { [%clk 0:00:57] } 42... Rd2 { [%clk 0:00:33] } 43. b3 { [%clk 0:00:51] } 43... Red7 { [%clk 0:00:27] } 44. Bxc5 { [%clk 0:00:48] } 44... Rd1 { [%clk 0:00:16] } 45. Bd4 { [%clk 0:00:41] } 45... Rxe1+ { [%clk 0:00:14] } 46. Qxe1 { [%clk 0:00:39] } 46... Qf7 { [%clk 0:00:10] } 47. Qe2 { [%clk 0:00:34] } 47... Nd5 { [%clk 0:00:06] } 48. Qf2 { [%clk 0:00:30] } 48... Bc6 { [%clk 0:00:01] } 49. Re1 { [%clk 0:00:27] } { White wins on time. } 1-0
  """.trim

  val pgn4 =
    "1. e4 { [%clk 0:10:00] } 1... c5 { [%clk 0:10:00] } 2. Nf3 { [%clk 0:09:57] } 2... Nc6 { [%clk 0:09:58] } 3. Bb5 { B30 Sicilian Defense: Nyezhmetdinov-Rossolimo Attack } 3... b6"
