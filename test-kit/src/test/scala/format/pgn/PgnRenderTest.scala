package chess
package format.pgn

import cats.syntax.all.*
import monocle.syntax.all.*

import scala.language.implicitConversions

class PgnRenderTest extends ChessTest:

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
    val results = pgns.map(Parser.full(_).get.toPgn.render).mkString("\n\n")
    assertEquals(results, output)

  test("wcc2023 study round trip tests"):
    val results = wcc2023.map(Parser.full(_).get.toPgn.render).mkString("\n\n")
    assertEquals(results, output2)

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

  val output =
    """
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
play with zero losing chances so an ideal choice for game one } 4... cxd5 5. Bf4 Nc6 6. e3 Bf5 7. Nf3 e6 { Black cannot continue symmetrically for
too long of course but this is the most solid choice } 8. Qb3 Bb4 9. Bb5 O-O { Black breaks the symmetry but this is still the main line of chess
opening theory } 10. Bxc6 (10. O-O Bxc3 11. Bxc6 Bxb2 12. Bxb7 Bxa1) 1/2-1/2

[Event "Live Chess"]
[Site "Chess.com"]
[Date "2016.05.10"]
[Round "?"]
[White "LyonBeast"]
[Black "FabianoCaruana"]
[Result "0-1"]
[WhiteElo "2747"]
[BlackElo "2699"]
[ECO "A00"]
[TimeControl "300+2"]
[Termination "FabianoCaruana won by resignation"]
[Annotator "Komodo 9.01 64-bit"]
[CurrentPosition "3r2k1/p3n1p1/1p5p/2q1p3/2P1B1R1/2Q1P3/P5PP/3r1R1K w - - 6 28"]

1. d4 Nf6 2. c4 e6 3. Nf3 d5 4. Nc3 Be7 5. Bg5 h6 6. Bh4 O-O 7. e3 Ne4 8. Bxe7 Qxe7 9. cxd5 Nxc3 10. bxc3 exd5 11. Qb3 Rd8 12. c4 Be6 13. Rc1 c5 14. dxc5 Qxc5 15. Nd4 Nc6 16. Nxe6 fxe6 17. Be2 d4 18. O-O dxe3 19. fxe3 Ne5 20. Kh1 b6 21. Qc3 Rd6 22. Rf4 Rad8 23. Re4 Nc6 24. Rf1 e5 25. Rg4 Rd2 26. Bf3 Ne7 27. Be4 Rd1 28. Kg1? (28. Rg1! { Forced, but Black is clearly much, much better. The silicon companion says Kh8 is now best. #shrug }) 28... b5? (28... R8d3! { This classic interference tactic just ends the game. } 29. Bxd3 Qxe3+ 30. Kh1 Rxf1+ 31. Bxf1 Qxc3) 29. Bc2? (29. Rxd1 Rxd1+ 30. Kf2 { Black clearly still has a big plus, but is not yet winning. }) 29... Rxf1+ 30. Kxf1 Nd5 31. Rxg7+ Kf8! { Black simply leaves White to deal with the hanging queen, e3 pawn, and rook. } (31... Kxg7? 32. Qxe5+ Kf8 33. Qh8+ Ke7 34. Qe5+ Kf7 35. cxd5 { Unclear. }) (31... Kh8? 32. Rh7+ Kg8 33. Rh8+!) 0-1

[Event "Live Chess - Crazyhouse"]
[Site "Chess.com"]
[Date "2016.11.30"]
[White "eekarf"]
[Black "JannLeeCrazyhouse"]
[Result "0-1"]
[ECO "C46"]
[WhiteElo "2071"]
[BlackElo "2593"]
[TimeControl "180"]
[Termination "JannLeeCrazyhouse won by checkmate"]
[Variant "Crazyhouse"]
[CurrentPosition "r1bBk1r1/ppp2p1p/1b1p3p/3Pp3/6N1/2PPn2n/PP2B1pP/R2Q3K w q - 0 22 QNrp"]

1. e4 e5 2. Nf3 Nc6 3. Nc3 Bc5 4. Be2 Nf6 5. O-O d6 6. d3 Nd4 7. Nxd4 Bxd4 8. Nd5 Nxd5 9. exd5 N@f6 10. c3 Bb6 11. Bg5 N@f4 12. N@e4 Rg8 13. N@h6 Nxe4 14. Bxd8 N@h3+ 15. gxh3 Nxh3+ 16. Kh1 Nexf2+ 17. Rxf2 Nxf2+ 18. Kg2 gxh6+ 19. N@g4 N@e3+ 20. Kg1 Nh3+ 21. Kh1 P@g2# 0-1

[Result "1-0"]

1. Nf3 d5 2. d4 c6 3. c4 e6 4. Nbd2 Nf6 5. e3 c5 6. b3 Nc6 7. Bb2 cxd4 8. exd4 Be7 9. Rc1 O-O 10. Bd3 Bd7 11. O-O Nh5 12. Re1 Nf4 13. Bb1 Bd6 14. g3 Ng6 15. Ne5 Rc8 16. Nxd7 Qxd7 17. Nf3 Bb4 18. Re3 Rfd8 19. h4 Nge7 20. a3 Ba5 21. b4 Bc7 22. c5 Re8 23. Qd3 g6 24. Re2 Nf5 25. Bc3 h5 26. b5 Nce7 27. Bd2 Kg7 28. a4 Ra8 29. a5 a6 30. b6 Bb8 31. Bc2 Nc6 32. Ba4 Re7 33. Bc3 Ne5 34. dxe5 Qxa4 35. Nd4 Nxd4 36. Qxd4 Qd7 37. Bd2 Re8 38. Bg5 Rc8 39. Bf6+ Kh7 40. c6 bxc6 41. Qc5 Kh6 42. Rb2 Qb7 43. Rb4 1-0

[FEN "8/rnbqkbnr/pppppppp/8/8/PPPPPPPP/RNBQKBNR/8 w - - 0 1"]

1. d4 d5 2. Nf4 Nf5 3. g4 g5 4. gxf5 exf5 5. Nbd3 gxf4 6. Nxf4 Bxf4 7. exf4 Nd6 8. Bh4 Qd8 9. Bd3 Kd7 10. Kf1 Kc8 11. Rg2 Kb8 12. Qe1 Bh5 13. Qf2 Rh8 14. Re2 Bf7 15. Rg3 a5 16. Rg2 a4 17. b4 Rf8 18. Re3 Nc4 19. Bxc4 dxc4 20. Kg1 Qd6 21. Qd2 Rc7 22. Kh2 b5 23. Rg7 Kb7 24. Re1 Bh5 25. Rxc7+ Kxc7 26. Qe2 Kd7 27. Rf1 Rg8 28. Qd2 Re8 29. Qf2 Qxf4+ 30. Kg2 Bxf3+ 31. Qxf3 Qxh4 32. Qxf5+ Kc7 33. Qf4+ Qxf4 34. Rxf4 Re2+ 35. Rf2 Re3 36. Rf3 Rd3 37. Kf2 Rd2+ 38. Ke1 Ra2 39. Rxf6 h5 40. Rf7+ Kd6 41. Rf5 Rxa3 42. Kd2 Ra2+ 43. Kc1 h4 44. Rf4 a3 45. Kb1 Rb2+ 46. Ka1 Kd5 47. Rxh4 Rc2 48. Rh8 Rxc3 49. Ka2 Rb3 50. Rd8+ Ke4 51. h4 Rxb4 52. Kxa3 Rb3+ 53. Ka2 Rh3 54. Rd6 b4 55. Rxc6 Kxd4 56. Rg6 Rh2+ 57. Kb1 c3 58. Kc1 Kc4 59. Kd1 Kb3 60. Rc6 c2+ 61. Kc1 Rh1+ 62. Kd2 Rd1+ 63. Ke2 c1=Q 64. Rxc1 Rxc1 65. Kf3 Rh1 66. Ke3 Kc4 67. Kf3 Rxh4 68. Kg2 b3 69. Kg3 Rd4 70. Kh3 b2 71. Kg3 b1=R 72. Kf3 Rb3+ 73. Ke2 Rdd3 74. Kf2 Rd2+ 75. Ke1 Ra2 76. Kd1 Rb1#

[Result "1-0"]

1. g4 e5 2. d4 e4 3. c4 Qh4 4. h3 Bb4+ 5. Nc3 Bxc3+ 6. bxc3 Qe7 7. Bf4 d6 8. e3 g5 9. Bg3 Be6 10. Rb1 Bc8 11. Be2 Nf6 12. h4 gxh4 13. Bxh4 Qe6 14. g5 Nfd7 15. Nh3 Rg8 16. Nf4 Qe7 17. Nd5 Qd8 18. g6 f6 19. gxh7 1-0

[Event "NII - Lille 2 / Tour Blanche Paris"]
[Site "Lille"]
[Date "2015.03.14"]
[Round "8"]
[White "Blazquez, Denis"]
[Black "Soubirou, Oriane"]
[Result "0-1"]
[ECO "B00"]
[WhiteElo "2083"]
[BlackElo "2135"]
[PlyCount "35"]

1. d4 a6 2. e4 e6 3. c4 b5 4. cxb5 axb5 5. Bxb5 Bb7 6. Nc3 Bb4 7. Qe2 Qh4 8. Bd3 f5 9. g3 Qf6 10. Nf3 c5 11. O-O Ne7 12. Bg5 Qf8 13. Nb5 Bxe4 14. Bxe4 fxe4 15. Qxe4 Nd5 16. Ne5 cxd4 17. Qxd5 Ra7 18. Qa8 0-1

[Event "NII - Lille 2 / Tour Blanche Paris"]
[Site "Lille"]
[Date "2015.03.14"]
[Round "8"]
[White "Blazquez, Denis"]
[Black "Soubirou, Oriane"]
[Result "0-1"]
[ECO "B00"]
[WhiteElo "2083"]
[BlackElo "2135"]
[PlyCount "35"]

1. d4 a6 2. e4 e6 3. c4 b5 4. cxb5 axb5 5. Bxb5 Bb7 6. Nc3 Bb4 7. Qe2 Qh4 8. Bd3 f5 9. g3 Qf6 10. Nf3 c5 11. O-O Ne7 12. Bg5 Qf8 13. Nb5 Bxe4 14. Bxe4 fxe4 15. Qxe4 Nd5 16. Ne5 cxd4 17. Qxd5 Ra7 18. Qa8 0-1

[Event "NII - Lille 2 / Tour Blanche Paris"]
[Site "Lille"]
[Date "2015.03.14"]
[Round "8"]
[White "Blazquez, Denis"]
[Black "Soubirou, Oriane"]
[Result "0-1"]
[ECO "B00"]
[WhiteElo "2083"]
[BlackElo "2135"]
[PlyCount "35"]

1. d4 a6 2. e4 e6 3. c4 b5 4. cxb5 axb5 5. Bxb5 Bb7 6. Nc3 Bb4 7. Qe2 Qh4 8. Bd3 f5 9. g3 Qf6 10. Nf3 c5 11. O-O Ne7 12. Bg5 Qf8 13. Nb5 Bxe4 14. Bxe4 fxe4 15. Qxe4 Nd5 16. Ne5 cxd4 17. Qxd5 Ra7 18. Qa8 0-1

[Event "Champions Showdown 2018"]
[Site "Saint Louis, United States"]
[Date "2018.09.12"]
[Round "6.1"]
[White "Kasparov, Garry"]
[Black "Topalov, Veselin"]
[Result "*"]
[ePGN "0.1;DGT LiveChess/2.2.3"]
[WhiteClock "00:04:08"]
[BlackClock "00:07:03"]
[ReferenceTime "B/2018-09-12T15:26:56.191-05:00"]
[FEN "rbnnqkbr/pppppppp/8/8/8/8/PPPPPPPP/RBNNQKBR w HAha - 0 1"]
[Variant "Fischerandom"]
[WhiteTitle "GM"]
[BlackTitle "GM"]
[WhiteElo "2734"]
[BlackElo "2722"]

1. d4 { [%clk 00:30:00] } { [%emt 00:00:05] } 1... d5 { [%clk 00:29:57] } { [%emt
00:00:14] } 2. f3 { [%clk 00:30:00] } { [%emt 00:00:04] } 2... f6 { [%clk 00:29:16] } { [%emt 00:00:51] } 3. Nd3 { [%clk 00:29:58] } { [%emt 00:00:13] } 3... c6 { [%clk
00:28:46] } { [%emt 00:00:40] } 4. e4 { [%clk 00:29:06] } { [%emt 00:01:02] } 4... dxe4 { [%clk 00:28:14] } { [%emt 00:00:43] } 5. fxe4 { [%clk 00:27:57] } { [%emt 00:01:18] } 5... e5 { [%clk 00:28:03] } { [%emt 00:00:20] } 6. dxe5 { [%clk 00:23:33] } { [%emt
00:04:34] } 6... Bxe5 { [%clk 00:26:44] } { [%emt 00:01:29] } 7. Bc5+ { [%clk 00:23:11] } { [%emt 00:00:32] } 7... Bd6 { [%clk 00:26:44] } { [%emt 00:00:03] } 8. Bxd6+ { [%clk
00:23:11] } { [%emt 00:00:03] } 8... Nxd6 { [%clk 00:26:44] } { [%emt 00:00:01] } 9. e5 { [%clk 00:23:11] } { [%emt 00:00:01] } 9... fxe5 { [%clk 00:25:53] } { [%emt 00:01:00] } 10. O-O+ { [%clk 00:22:47] } { [%emt 00:00:34] } 10... Bf7 { [%clk 00:23:55] } { [%emt
00:02:09] } 11. Nxe5 { [%clk 00:22:13] } { [%emt 00:00:44] } 11... O-O { [%clk 00:23:52] } { [%emt 00:00:12] } 12. c3 { [%clk 00:22:13] } { [%emt 00:00:02] } 12... Qe7 { [%clk
00:18:39] } { [%emt 00:05:21] } 13. Nxf7 { [%clk 00:19:46] } { [%emt 00:02:38] } 13... Rxf7 { [%clk 00:18:19] } { [%emt 00:00:30] } 14. Bc2 { [%clk 00:19:35] } { [%emt 00:00:22] } 14... Rxf1+ { [%clk 00:18:06] } { [%emt 00:00:22] } 15. Qxf1 { [%clk 00:19:32] } { [%emt
00:00:13] } 15... N8f7 { [%clk 00:18:06] } { [%emt 00:00:08] } 16. Nf2 { [%clk 00:19:26] } { [%emt 00:00:15] } 16... Re8 { [%clk 00:18:06] } { [%emt 00:00:08] } 17. Rd1 { [%clk
00:15:06] } { [%emt 00:04:30] } 17... Ne5 { [%clk 00:13:44] } { [%emt 00:04:33] } 18. Nd3 { [%clk 00:12:31] } { [%emt 00:02:46] } 18... Nec4 { [%clk 00:07:44] } { [%emt 00:06:10] } 19. Re1 { [%clk 00:12:14] } { [%emt 00:00:26] } 19... Qf8 { [%clk 00:07:18] } { [%emt 00:00:36] } 20. Bb3 { [%clk 00:04:16] } { [%emt 00:08:08] } 20... Qxf1+ { [%clk 00:07:18] } { [%emt
00:00:06] } 21. Kxf1 { [%clk 00:04:16] } { [%emt 00:00:02] } 21... Kf8 { [%clk 00:07:18] } { [%emt 00:00:02] } 22. Rxe8+ { [%clk 00:04:16] } { [%emt 00:00:03] } 22... Kxe8 { [%clk
00:07:18] } { [%emt 00:00:01] } 23. Ke2 { [%clk 00:04:16] } { [%emt 00:00:02] } 23... Ke7 { [%clk 00:07:18] } { [%emt 00:00:03] } 24. Bxc4 { [%clk 00:04:08] } { [%emt
00:00:18] } 24... Nxc4 { [%clk 00:07:18] } { [%emt 00:00:02] } 25. b3 { [%clk 00:04:08] } 25... Nd6 { [%clk 00:07:18] } { [%emt 00:00:10] } 26. Ke3 { [%clk 00:04:08] } { [%emt
00:00:03] } *

[Event "F/S Return Match"]
[Site "Belgrade, Serbia Yugoslavia|JUG"]
[Date "1992.11.04"]
[Round "29"]
[White "Fischer, Robert J."]
[Black "Spassky, Boris V."]
[Result "1/2-1/2"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 { This opening is called the Ruy Lopez. } 3... a6 { this is an inline comment } 4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8 10. d4 Nbd7 11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 15. Nb1 h6 16. Bh4 c5 17. dxe5 { Openning route to ocupying b6 weak square by Na4-Nb6. This square seemed more important than f5 (Ne2-Ng3-Nf5) because its nearer the black's king. } 17... Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21. Nc4 Nxc4 22. Bxc4 Nb6 23. Ne5 Rae8 24. Bxf7+ Rxf7 25. Nxf7 Rxe1+ 26. Qxe1 Kxf7 27. Qe3 Qg5 28. Qxg5 hxg5 29. b3 Ke6 30. a3 Kd6 31. axb4 cxb4 32. Ra5 Nd5 33. f3 Bc8 34. Kf2 Bf5 35. Ra7 g6 36. Ra6+ Kc5 37. Ke1 Nf4 38. g3 Nxh3 39. Kd2 Kb5 40. Rd6 Kc5 41. Ra6 Nf2 42. g4 Bd3 43. Re6 1/2-1/2

[Event "The Match - Braingames World Chess Cham"]
[Site "London"]
[Date "2000.01.04"]
[Round "3"]
[White "Garry Kasparov"]
[Black "Vladimir Kramnik"]
[Result "1/2-1/2"]
[EventDate "2000.10.12"]
[ECO "C67"]
[WhiteElo "2849"]
[BlackElo "2770"]
[PlyCount "106"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 Nf6 4. O-O Nxe4 5. d4 Nd6 6. Bxc6 dxc6 7. dxe5 Nf5 8. Qxd8+ Kxd8 9. Nc3 Bd7 10. b3 h6 11. Bb2 Kc8 12. Rad1 b6 13. Ne2 c5 14. c4 Bc6 15. Nf4 Kb7 16. Nd5 Ne7 17. Rfe1 Rg8 18. Nf4 g5 19. Nh5 Rg6 20. Nf6 Bg7 21. Rd3 Bxf3 22. Rxf3 Bxf6 23. exf6 Nc6 24. Rd3 Rf8 25. Re4 Kc8 26. f4 gxf4 27. Rxf4 Re8 28. Bc3 Re2 29. Rf2 Re4 30. Rh3 a5 31. Rh5 a4 32. bxa4 Rxc4 33. Bd2 Rxa4 34. Rxh6 Rg8 35. Rh7 Rxa2 36. Rxf7 Ne5 37. Rg7 Rf8 38. h3 c4 39. Re7 Nd3 40. f7 Nxf2 41. Re8+ Kd7 42. Rxf8 Ke7 43. Rc8 Kxf7 44. Rxc7+ Ke6 45. Be3 Nd1 46. Bxb6 c3 47. h4 Ra6 48. Bd4 Ra4 49. Bxc3 Nxc3 50. Rxc3 Rxh4 51. Rf3 Rh5 52. Kf2 Rg5 53. Rf8 Ke5 1/2-1/2

[Event "Hastings"]
[Site "Hastings ENG"]
[Date "1895.08.05"]
[Round "1"]
[White "Carl Schlechter"]
[Black "William Henry Krause Pollock"]
[Result "1/2-1/2"]
[EventDate "1895.08.05"]
[ECO "C77"]
[WhiteElo "?"]
[BlackElo "?"]
[PlyCount "47"]

1. e4 { Notes by E. Schiffers } 1... e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. Nc3 Bb4 { The 'Handbuch' considers that Black's best
move here is Be7; White continues 6 O-O b5 7 Bb3 d6 8 a3 0r
a4. } 6. Nd5 Bc5 { After Be7 would follow 7 d3 h6 and an even
game. It would not be good to play ...Nxd5 7 exd5 Ne7 8 c3 Ba5
(?) 9 Nxe5. } 7. d3 { The 'Handbuch' gives the continuation 7 c3
Nxe4 8 d4 exd4 9 cxd4 Bb4+ 10 Kf1 in White's favour. } 7... h6 8. Be3 Bxe3 { Also good would have been Nxe3. } 9. fxe3 d6 10. O-O Be6 { ...Bg4 is better. } 11. Nxf6+ { An enterprising
move } 11... Qxf6 12. Nd4 Qg5 13. Nxc6 { After 13 Nxe6 fxe6 (...Qxe3+
14 Kh1 fxe6 15 Qh5+) 14 Bxc6+ bxc6 15 Qf3; White's game is
preferable. } 13... Qxe3+ 14. Kh1 Bd7 15. Nxe5 { And other moves still
do not give White the superiority, e.g. 15 Nb4 Bxa4 16 Nd5
Qc5, etc. } 15... Bxa4 16. Nxf7 O-O $14 { ! } 17. Qh5 Be8 18. Rf3 Bxf7 19. Qxf7+ Rxf7 20. Rxe3 Rf2 21. Rae1 { 21 Rc1 Raf8 followed by
...Rd2 and the other rook to f2 would have been worse as White
would then get into difficulties, whereas now the draw is
secure. } 21... Raf8 22. Kg1 Rxc2 23. R3e2 Rxe2 24. Rxe2 1/2-1/2

[Event "Live Chess"]
[Site "Chess.com"]
[Date "2014.01.15"]
[Round "?"]
[White "amarkaur"]
[Black "ludocode"]
[Result "0-1"]
[WhiteElo "1116"]
[BlackElo "1220"]
[Annotator "Crafty v23.4"]

{ annotating both black and white moves. } { using a scoring margin of +0.50 pawns. } { search time limit is 1.00 }
1. Nf3 d5 2. d4 Nc6 3. e3 Nf6 4. c4 dxc4 5. Bxc4 Qd6 ( { 15:+0.88 }5... Qd6 6. Nc3 Qb4 7. Nd2 e6 8. a3 Qa5 9. O-O Bd6 10. Nde4 Nxe4 11. Nxe4 O-O 12. Bd2 Qf5 13. Nxd6 cxd6 $16) ( { 15:+0.36 }5... e6 6. O-O Bd6 7. Nc3 O-O 8. e4 e5 9. d5 Na5 10. Bd3 Bd7 11. Bg5 Re8 12. Be3 Qe7 $14) 6. Nc3 e5 7. O-O exd4 8. Nxd4 ( { 13:+1.22 }8. Nxd4 Ne5 9. Be2 Neg4 10. f4 c5 11. Ndb5 Qe7 12. e4 c4 13. Qa4 Nxe4 14. Nxe4 Qxe4 15. Bxc4 $16) ( { 13:+2.05 }8. exd4 Bg4 9. Nb5 Qd7 10. Bf4 O-O-O 11. Bxc7 Bxf3 12. gxf3 Re8 13. d5 Nb4 14. Rc1 $18) 8... Nxd4 9. exd4 Be6 ( { 13:+1.32 }9... Be6 10. d5 Bd7 11. Re1+ Be7 12. Nb5 Bxb5 13. Bxb5+ Kf8 14. Bc4 Rd8 15. Bg5 Qc5 16. Bxf6 Bxf6 $16) ( { 13:+0.70 }9... Bd7 10. Re1+ Be7 11. Nb5 Bxb5 12. Bxb5+ c6 13. Bc4 O-O 14. Qb3 b5 15. Bd3 Rfe8 16. Be3 Nd5 $14) 10. Re1 ( { 13:+0.52 }10. Re1 O-O-O 11. Bxe6+ fxe6 12. Bg5 Rd7 13. Bxf6 gxf6 14. Qb3 Qxd4 15. Rad1 Qb6 16. Qxb6 axb6 17. Rxe6 Rxd1+ 18. Nxd1 $14) ( { 13:+1.32 }10. d5 Bd7 11. Nb5 Qb6 12. Re1+ Kd8 13. Qd3 Bc5 14. Be3 Bxe3 15. Rxe3 Re8 16. Ree1 Kc8 17. Rxe8+ Bxe8 $16) 10... O-O-O 11. Be2 ( { 15:-0.76 }11. Be2 Qxd4 12. Qxd4 Rxd4 13. Be3 Rb4 14. Rab1 Bf5 15. a3 Bxb1 16. axb4 Bf5 17. b5 Kb8 18. Rd1 $17) ( { 15:+0.49 }11. Bxe6+ fxe6 12. Bg5 Qxd4 13. Qxd4 Rxd4 14. Rxe6 Rd6 15. Re5 h6 16. Bxf6 Rxf6 17. Re8+ Kd7 18. Rae1 Rd6 $14) 11... Qxd4 12. Be3 Qb4 13. Qc1 Bd6 14. a3 Qh4 15. g3 Qh3 16. Bf3 ( { 14:-2.01 }16. Bf3 Ng4 17. Bxg4 Bxg4 18. f4 a6 19. Qc2 Rhe8 20. Ne4 Bf5 21. Nxd6+ Rxd6 22. Qg2 Qg4 $19) ( { 14:+0.01 }16. Bxa7 Bd7 17. Bf1 Qf5 18. Qd1 Bc6 19. Bd3 Qh3 20. Bf1 Qf5 $10) 16... Ng4 17. Bxg4 Bxg4 18. f3 ( { 15:-5.02 }18. f3 Bxf3 19. Re2 Bxe2 20. Nxe2 Rhe8 21. Nf4 Qg4 22. Ng2 b6 23. Qc2 Be5 24. Rc1 Rd5 25. Qc6 $19) ( { 15:-1.82 }18. f4 f5 19. Nb5 a6 20. Nxd6+ Rxd6 21. Qc2 Bf3 22. Rac1 Bc6 23. Bc5 Rh6 24. Re7 Be4 25. Qd2 $19) 18... Bxg3 19. hxg3 ( { 14:-Mat06 }19. hxg3 Qxg3+ 20. Kf1 Qxf3+ 21. Kg1 Qg3+ 22. Kf1 Bh3+ 23. Ke2 Qg4+ 24. Kf2 Qg2# $19) ( { 14:-6.51 }19. Re2 Bxf3 20. hxg3 Qxg3+ 21. Kf1 Bg4 22. Rd2 Rxd2 23. Qxd2 Bh3+ 24. Ke2 Qg2+ 25. Kd1 Rd8 26. Bd4 Qf1+ 27. Qe1 Rxd4+ 28. Kc2 Bf5+ 29. Kb3 $19) 19... Qxg3+ 20. Kf1 Bh3+ ( { 9:-9.33 }20... Bh3+ 21. Ke2 Qh2+ 22. Bf2 Rhe8+ 23. Qe3 Rxe3+ 24. Kxe3 Qe5+ 25. Ne4 f5 26. Bh4 f4+ 27. Kf2 Qd4+ 28. Ke2 Qxb2+ $19) ( { 9:-Mat05 }20... Qxf3+ 21. Kg1 Qg3+ 22. Kf1 Bh3+ 23. Ke2 Qg4+ 24. Kf2 Qg2# $19) 21. Ke2 Qg2+ 22. Bf2 Rhe8+ 23. Ne4 Bg4 ( { 13:-9.33 }23... Bg4 24. Qf4 Bxf3+ 25. Qxf3 Rxe4+ 26. Qxe4 Qxe4+ 27. Kf1 Qh1+ 28. Ke2 Qh5+ 29. Kf1 Rd2 30. Kg1 Qg4+ 31. Kf1 Rxb2 32. Re7 $19) ( { 13:-Mat07 }23... Rxe4+ 24. Qe3 Rxe3+ 25. Kxe3 Qg5+ 26. f4 Qc5+ 27. Ke2 Qh5+ 28. Ke3 Bg2 29. Bh4 Qf3# $19) 24. Qe3 f5 ( { 15:-4.06 }24... f5 25. Rg1 Bxf3+ 26. Ke1 Bxe4 27. Rxg2 Bxg2 28. Rd1 Rxe3+ 29. Bxe3 Rxd1+ 30. Kxd1 g6 31. Kd2 a5 $19) ( { 15:-11.88 }24... Bxf3+ 25. Qxf3 Rxe4+ 26. Qxe4 Qxe4+ 27. Kf1 Qh1+ 28. Ke2 Qh5+ 29. Kf1 Qh3+ 30. Kg1 Rd6 31. Re8+ Kd7 32. Re3 Rg6+ 33. Bg3 Rxg3+ 34. Rxg3 Qxg3+ 35. Kh1 Qf3+ 36. Kg1 Qe3+ 37. Kh1 Qe4+ 38. Kh2 $19) 25. Rg1 Bxf3+ 26. Qxf3 ( { 15:-18.62 }26. Qxf3 Rxe4+ 27. Qxe4 Qxe4+ 28. Kf1 Qd3+ 29. Kg2 Rd6 30. Rge1 Rg6+ 31. Kh2 Qf3 32. Re8+ Kd7 33. Re7+ Kxe7 34. Re1+ Kf7 35. Re7+ Kxe7 36. Bc5+ Ke6 37. Bxa7 $19) ( { 15:-4.05 }26. Ke1 Bxe4 27. Rxg2 Bxg2 28. Qxe8 Rxe8+ 29. Kd2 g6 30. Rg1 Be4 31. Be3 b6 32. Bf4 Rd8+ 33. Ke2 c5 34. Rg5 $19) 26... Rxe4+ 27. Qxe4 Qxe4+ 28. Kf1 Qd3+ 29. Kg2 Qb3 ( { 13:-9.38 }29... Qb3 30. Rge1 Rd2 31. Re8+ Kd7 32. Rae1 Qxb2 33. R8e7+ Kd6 34. R1e6+ Kd5 35. Re5+ Qxe5 36. Rd7+ Qd6 37. Rxd6+ Kxd6 $19) ( { 13:-22.79 }29... Rd6 30. Rge1 Rg6+ 31. Kh2 Qf3 32. Re8+ Kd7 33. Rd8+ Kxd8 34. Rd1+ Qxd1 35. Bh4+ Kd7 36. Bg3 Qe2+ 37. Kh3 Rh6+ 38. Bh4 Qe3+ 39. Kg2 Rxh4 $19) 30. Rgc1 ( { 12:-9.97 }30. Rgc1 Rd2 31. Rf1 Rxb2 32. Kg1 Qf3 33. Rae1 Qg4+ 34. Kh2 c5 35. Re3 Kc7 36. Kh1 Qh5+ 37. Kg2 $19) ( { 12:-9.38 }30. Rae1 Rd2 31. Re8+ Kd7 32. Rge1 Qxb2 33. R8e7+ Kd6 34. R1e6+ Kd5 35. Re5+ Qxe5 36. Rd7+ Qd6 37. Rxd6+ Kxd6 $19) 30... Qxb2 ( { 13:-9.47 }30... Qxb2 31. Rd1 Rxd1 32. Rxd1 Qxa3 33. Bh4 Qa2+ 34. Kg1 c5 35. Re1 Qa4 36. Bg3 Qd4+ 37. Kg2 Kd7 $19) ( { 13:-12.77 }30... Rd2 31. Rh1 Qc2 32. Rhf1 Qe4+ 33. Kh2 Qh4+ 34. Kg2 Qg4+ 35. Kh1 Qh3+ 36. Kg1 Rd6 37. Bh4 Rg6+ 38. Kf2 Rg2+ 39. Ke1 Qxh4+ 40. Kd1 $19) 31. Kg1 ( { 14:-15.11 }31. Kg1 Rd6 32. Re1 Rg6+ 33. Kf1 Qb5+ 34. Re2 Qd5 35. Re8+ Kd7 36. Re3 Qh1+ 37. Ke2 Qxa1 38. Rd3+ Kc6 39. Bxa7 Rg2+ 40. Bf2 Qa2+ 41. Rd2 Qxa3 $19) ( { 14:-9.45 }31. Rd1 Rxd1 32. Rxd1 Qxa3 33. Rh1 Qa6 34. Rh4 c5 35. Kf3 Qd3+ 36. Be3 Qd5+ 37. Kg3 h6 38. Ra4 Qb3 $19) 31... Rd2 ( { 13:-10.53 }31... Rd2 32. Rcb1 Qc2 33. Rf1 Qa4 34. Rae1 Qg4+ 35. Kh2 f4 36. Kh1 Kd7 37. a4 Qh5+ 38. Kg2 Qd5+ 39. Kh2 Qh5+ 40. Kg2 $19) ( { 13:-15.11 }31... Rd6 32. Re1 Rg6+ 33. Kf1 Qb5+ 34. Re2 Qd5 35. Re8+ Kd7 36. Re3 Qh1+ 37. Ke2 Qxa1 38. Rd3+ Kc6 39. Bxa7 Rg2+ 40. Bf2 Qa2+ 41. Rd2 Qxa3 $19) 32. Rab1 Qf6 ( { 15:-12.64 }32... Qf6 33. Rb3 Qg5+ 34. Rg3 Qf4 35. Rg2 Rxf2 36. Rxc7+ Kxc7 37. Rxf2 Qg3+ 38. Rg2 Qe1+ 39. Kh2 g5 40. Rg3 Qe2+ 41. Rg2 Qe5+ 42. Rg3 $19) ( { 15:-13.99 }32... Qxa3 33. Rd1 Ra2 34. Rd4 Rxf2 35. Kxf2 Qa2+ 36. Ke3 Qxb1 37. Kd2 Qb2+ 38. Kd3 Qb3+ 39. Kd2 g5 40. Rd3 Qb2+ 41. Ke3 g4 $19) 33. Bg3 ( { 14:-Mat04 }33. Bg3 Qd4+ 34. Kf1 Qd3+ 35. Kg1 Qxg3+ 36. Kh1 Rh2# $19) ( { 14:-12.64 }33. Rb3 Qg5+ 34. Rg3 Qf4 35. Rg2 Rxf2 36. Rxc7+ Kxc7 37. Rxf2 Qg3+ 38. Rg2 Qe1+ 39. Kh2 g5 40. Rg3 Qe2+ 41. Rg2 Qe5+ 42. Rg3 $19) 33... Qg5 ( { 7:-10.72 }33... Qg5 34. Rxc7+ Kd8 35. Rc3 f4 36. Rxb7 fxg3 37. Rxa7 $19) ( { 7:-Mat04 }33... Qd4+ 34. Kf1 Qd3+ 35. Kg1 Qxg3+ 36. Kh1 Rh2# $19) 34. Rxc7+ Kd8 0-1

[Event "nTCEC - Stage 2 - Season 2"]
[Site "http://www.tcec-chess.net"]
[Date "2013.10.09"]
[Round "13.9"]
[White "Komodo 1092"]
[Black "Stockfish 160913"]
[Result "1-0"]
[WhiteElo "3113"]
[BlackElo "3110"]
[Time "05:39:46"]
[ECO "D10"]
[Opening "Slav: 3.Nc3 Nf6 4.e3"]
[TimeControl "7200+30"]
[PlyCount "165"]
[Number "117"]
[Termination "GUI adjudication"]
[WhiteType "program"]
[BlackType "program"]
[Variant "normal"]

{ 2 x Intel Xeon E5-2689 }
1. d4 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=d4, tb=0, R50=50, wv=0.00, } 1... d5 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=d5, tb=0, R50=50, wv=0.00, } 2. c4 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=c4, tb=0, R50=50, wv=0.00, } 2... c6 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=c6, tb=0, R50=50, wv=0.00, } 3. Nc3 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=Nc3, tb=0, R50=49, wv=0.00, } 3... Nf6 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=Nf6, tb=0, R50=49, wv=0.00, } 4. e3 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=e3, tb=0, R50=50, wv=0.00, } 4... g6 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=g6, tb=0, R50=50, wv=0.00, } 5. Nf3 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=Nf3, tb=0, R50=49, wv=0.00, } 5... Bg7 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=Bg7, tb=0, R50=49, wv=0.00, } 6. Be2 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=Be2, tb=0, R50=48, wv=0.00, } 6... O-O { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=O-O, tb=0, R50=48, wv=0.00, } 7. O-O { ev=0.26, d=22, pd=Be6, mt=00:01:39, tl=01:58:50, s=8705 kN/s, n=867251059, pv=O-O Be6 b3 a6 Bd2 Bf5 h3 Nbd7 Rc1 Re8 g4 Be6 Ng5 Nf8 f4 h6 Nxe6 Nxe6 cxd5 cxd5 h4 Qd6 g5 Ne4 Nxe4, tb=0, R50=47, wv=0.26, } 7... Bg4 { ev=-0.44, d=32, pd=Qb3, mt=00:02:29, tl=01:58:00, s=17364 kN/s, n=2599449979, pv=Bg4 Qb3 Qb6 Bd2 e6 h3 Bxf3 Bxf3 Nbd7 Rfd1 Rfe8 Be2 dxc4 Bxc4 Qxb3 axb3 Red8 g4 Nb6 Kg2 Nfd5 Nxd5 Nxd5 Ba5 Nb6, tb=0, R50=47, wv=0.44, } 8. Qb3 { ev=0.36, d=25, pd=Qb6, mt=00:02:59, tl=01:56:21, s=15490 kN/s, n=2790759992, pv=Qb3 Qb6 h3 Bxf3 Bxf3 dxc4 Qxc4 Nbd7 b4 e5 a4 a6 Rd1 Qc7 a5 Ne8 Qb3 Rd8 d5 e4 Be2 Ne5 Bb2 Nf6 Rac1, tb=0, R50=46, wv=0.36, } 8... Qb6 { ev=-0.48, d=31, pd=h3, mt=00:01:26, tl=01:57:05, s=18266 kN/s, n=1576985656, pv=Qb6 h3 Bxf3 Bxf3 e6 Qa4 Qa6 Qxa6 Nxa6 Rb1 Nc7 b3 Rfe8 Bb2 Bf8 Rfc1 Kg7 g3 Bb4 Kg2 Rad8 a3 Be7 c5 Nd7, tb=0, R50=46, wv=0.48, } 9. h3 { ev=0.34, d=25, pd=Bxf3, mt=00:02:01, tl=01:54:50, s=17533 kN/s, n=2135695329, pv=h3 Bxf3 Bxf3 e6 Bd2 Nbd7 Rfd1 Rfd8 Be2 Qc7 Rac1 Nb6 cxd5 exd5 Bd3 a5 a4 Nc8 f3 Qe7 Ne2 Nd6 Qc3 Bh6 Nf4, tb=0, R50=50, wv=0.34, } 9... Bxf3 { ev=-0.38, d=34, pd=Bxf3, mt=00:01:20, tl=01:56:15, s=17740 kN/s, n=1422282657, pv=Bxf3 Bxf3 e6 Rd1 Nbd7 Bd2 Rfd8 Be2 dxc4 Bxc4 Qxb3 axb3 Nb6 Be2 Nbd5 g4 Bf8 h4 Be7 g5 Nxc3 bxc3 Ne4 Be1 Nd6, tb=0, R50=50, wv=0.38, } 10. Bxf3 { ev=0.25, d=24, pd=e6, mt=00:00:57, tl=01:54:24, s=17221 kN/s, n=992438519, pv=Bxf3 e6 Na4 Qc7 cxd5 exd5 Nc5 b6 Na4 Re8 Nc3 Na6 Bd2 Qd6 Rfc1 Nc7 Rc2 Ne6 Rac1 Rad8 Be2 b5 Bd3 Ng5 a4, tb=0, R50=50, wv=0.25, } 10... e6 { ev=-0.38, d=33, pd=Rd1, mt=00:02:52, tl=01:53:54, s=18817 kN/s, n=3240609540, pv=e6 Rd1 Nbd7 Bd2 Rfb8 Be2 dxc4 Bxc4 Qxb3 axb3 Rd8 g4 Nd5 Be2 Bf6 Kg2 Kg7 Ne4 Be7 g5 h6 h4 hxg5 hxg5 a6, tb=0, R50=50, wv=0.38, } 11. Bd2 { ev=0.25, d=25, pd=Nbd7, mt=00:04:36, tl=01:50:18, s=14418 kN/s, n=3989679346, pv=Bd2 Nbd7 Rfd1 Rfd8 Rac1 Rac8 Be2 dxc4 Bxc4 Qxb3 axb3 Nb6 Be2 Nbd5 Ra1 a6 Bf3 Nd7 Ra5 Ra8 Ra4 N5b6 Raa1 f5 Ne2, tb=0, R50=49, wv=0.25, } 11... Nbd7 { ev=-0.42, d=34, pd=Rfd1, mt=00:01:16, tl=01:53:08, s=17892 kN/s, n=1360474566, pv=Nbd7 Rfd1 Rfb8 Be2 dxc4 Bxc4 Qxb3 axb3 Rd8 g4 Nd5 Kg2 Bf6 Ne4 Be7 g5 Kg7 h4 a6 Rac1 Rac8 Be2 Rf8 Bf3 f5, tb=0, R50=49, wv=0.42, } 12. Rfd1 { ev=0.26, d=25, pd=Rfd8, mt=00:02:26, tl=01:48:23, s=16901 kN/s, n=2486621695, pv=Rfd1 Rfd8 Rac1 Rac8 Be2 dxc4 Bxc4 Qxb3 axb3 Nb6 Be2 Bf8 Bf3 Nfd5 g3 Nb4 Be2 f5 Kg2 Ra8 Na4 N6d5 Nc5 Bxc5 dxc5, tb=0, R50=48, wv=0.26, } 12... Rfb8 { ev=-0.40, d=33, pd=Rab1, mt=00:07:49, tl=01:45:49, s=19416 kN/s, n=9120778899, pv=Rfb8 Rab1 Qxb3 axb3 Nb6 Be2 Re8 g4 e5 dxe5 Rxe5 Kg2 Ne4 Nxe4 dxe4 Bb4 Ree8 Bd6 Be5 Bxe5 Rxe5 h4 Kg7 Ra1 a6, tb=0, R50=48, wv=0.40, } 13. Be2 { ev=0.31, d=25, pd=Qc7, mt=00:04:42, tl=01:44:11, s=8437 kN/s, n=2390228202, pv=Be2 Qc7 Qc2 Rd8 Rac1 dxc4 Bxc4 Nb6 Bd3 Qd6 a3 e5 Ne2 exd4 Nxd4 Nfd5 Nf3 Rd7 h4 Re8 h5 Red8 Be2 Re8 hxg6, tb=0, R50=47, wv=0.31, } 13... dxc4 { ev=-0.38, d=32, pd=Bxc4, mt=00:02:11, tl=01:44:08, s=20023 kN/s, n=2625161804, pv=dxc4 Bxc4 Qxb3 axb3 Rd8 Be2 Nd5 g4 Bf6 Ne4 Be7 g5 Kg7 h4 h5 Kg2 a6 Ba5 Rdc8 Bc3 Re8 Bd3 Rad8 Ba5 Rc8, tb=0, R50=50, wv=0.38, } 14. Bxc4 { ev=0.39, d=23, pd=Qxb3, mt=00:01:41, tl=01:43:00, s=17663 kN/s, n=1803476132, pv=Bxc4 Qxb3 axb3 Rd8 g4 Nb6 Be2 Nbd5 Kg2 Ne8 Nxd5 exd5 Ba5 Rd7 h4 Nd6 Bb6 a6 h5 Bf6 Bd3 Re8 Rh1 Ne4 b4, tb=0, R50=50, wv=0.39, } 14... Qxb3 { ev=-0.44, d=33, pd=axb3, mt=00:09:14, tl=01:35:24, s=20337 kN/s, n=11276634044, pv=Qxb3 axb3 Rd8 e4 b5 Bd3 Nc5 Bxb5 Rxd4 Be3 Rxd1 Rxd1 cxb5 Bxc5 Rc8 b4 a6 e5 Ne8 f4 f6 exf6 Bxf6 Kf2 Rd8, tb=0, R50=50, wv=0.44, } 15. axb3 { ev=0.33, d=26, pd=Rd8, mt=00:03:31, tl=01:39:59, s=15744 kN/s, n=3341606297, pv=axb3 Rd8 g4 a6 g5 Nd5 Ne4 Bf8 Kg2 h5 h4 Be7 Be2 Kg7 Ba5 Rh8 Bc3 Rhf8 Rg1 Rac8 Bd3 Rb8 Ba5 Bb4 Rgd1, tb=0, R50=50, wv=0.33, } 15... Ne8 { ev=-0.44, d=31, pd=g4, mt=00:01:46, tl=01:34:08, s=13322 kN/s, n=2151364497, pv=Ne8 g4 Nd6 Be2 Nb6 f3 Rc8 Kg2 a6 Ne4 Nxe4 fxe4 Bf6 Kg3 Bd8 Bc3 Bc7 Kg2 Bd8 Kf3 Nd7 Kg3 Bg5 d5 cxd5, tb=0, R50=49, wv=0.44, } 16. Be2 { ev=0.47, d=25, pd=Nc7, mt=00:03:29, tl=01:37:01, s=18493 kN/s, n=3871138292, pv=Be2 Nc7 g3 Nd5 Kg2 Re8 e4 Nb4 Rac1 a5 Bf4 Nf6 Bg5 h6 Bd2 Red8 Bf4 Nd7 Bc7 Rdc8 Bd6 Bf8 Bf4 Kg7 e5, tb=0, R50=49, wv=0.47, } 16... a6 { ev=-0.38, d=32, pd=g3, mt=00:04:06, tl=01:30:32, s=20400 kN/s, n=5029048486, pv=a6 g3 f5 Kg2 Nd6 Na4 Kf7 f3 Re8 Ba5 Nf6 Nb6 Nd5 Kf2 Bh6 Rd3 Nxb6 Bxb6 Nb5 Rdd1 Bf8 Ba5 Be7 h4 Bd6, tb=0, R50=50, wv=0.38, } 17. Na4 { ev=0.47, d=24, pd=a5, mt=00:03:26, tl=01:34:05, s=18245 kN/s, n=3769943507, pv=Na4 a5 g3 Nc7 Kg2 Nd5 Nc3 Nb4 Ne4 Bf8 Bc3 Be7 Rdc1 Rd8 Nd2 Bd6 e4 Bc7 Ra3 Bb6 Nc4 Bc7 e5 h5 Nd2, tb=0, R50=49, wv=0.47, } 17... f5 { ev=-0.30, d=32, pd=g3, mt=00:01:58, tl=01:29:04, s=19974 kN/s, n=2375071266, pv=f5 g3 Kf7 Be1 Nef6 b4 Nd5 Nc5 N7f6 Bd3 Ne8 Bd2 Nd6 f3 Re8 Kg2 Nc7 Rdc1 Rad8 Nxb7 Nxb7 Rxc6 Re7 Rac1 Rdd7, tb=0, R50=50, wv=0.30, } 18. b4 { ev=0.41, d=25, pd=Kf7, mt=00:04:42, tl=01:29:54, s=13034 kN/s, n=3672809064, pv=b4 Kf7 Nc5 Nb6 g3 Nd6 f3 Re8 Bc1 Re7 e4 h6 Bf4 Rd8 b3 Nb5 Bxb5 axb5 Be5 Kg8 Kg2 fxe4 fxe4 Nc8 Nd3, tb=0, R50=50, wv=0.41, } 18... Kf7 { ev=-0.30, d=34, pd=Bd3, mt=00:03:46, tl=01:25:48, s=20741 kN/s, n=4699142749, pv=Kf7 Bd3 Nd6 g3 Re8 Nc5 Nb6 Bc2 Rab8 Bb3 Ne4 Nxe4 fxe4 Kg2 Nd5 f3 Bh6 f4 Bf8 Kf2 Nxb4 Bxb4 Bxb4 Bc2 Kf6, tb=0, R50=49, wv=0.30, } 19. Nc5 { ev=0.47, d=24, pd=Nb6, mt=00:04:59, tl=01:25:25, s=19256 kN/s, n=5784859753, pv=Nc5 Nb6, tb=0, R50=49, wv=0.47, } 19... Nb6 { ev=-0.42, d=33, pd=g3, mt=00:01:19, tl=01:25:00, s=21132 kN/s, n=1680669218, pv=Nb6 g3 Nd6 Bd3 Nd5 f3 Re8 Kg2 Nb6 Rac1 Rab8 Re1 Bh6 Kf2 Bg7 Bc2 Bf6 Bc3 Nb5 Kg2 Bg5 h4 Bh6 Rcd1 Nd5, tb=0, R50=48, wv=0.42, } 20. e4 { ev=0.54, d=26, pd=Bxd4, mt=00:04:22, tl=01:21:34, s=18725 kN/s, n=4921124085, pv=e4 Bxd4 exf5 gxf5 Bf4 e5 Bh5 Kg8 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Bf3 Kf7 g3 Kg7 Kg2 Kg6 g4 Nb5 Rd1, tb=0, R50=50, wv=0.54, } 20... Bxd4 { ev=-0.34, d=32, pd=exf5, mt=00:01:18, tl=01:24:11, s=18657 kN/s, n=1469574656, pv=Bxd4 exf5 gxf5 Bh5 Kg8 Bf4 e5 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Rd1 Kg7 Bf3 Kh6 g3 Kg6 Kg2 Kg5 h4 Kh6, tb=0, R50=50, wv=0.34, } 21. exf5 { ev=0.53, d=26, pd=gxf5, mt=00:02:35, tl=01:19:29, s=20845 kN/s, n=3247118248, pv=exf5 gxf5 Bh5 Kg8 Bf4 e5 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nc7 Rxd4 Nb5 Rd1 Nd5 Bf3 Nbc7 g4 fxg4 hxg4 Nxb4 Rd7 Ncd5 Nxb7, tb=0, R50=50, wv=0.53, } 21... gxf5 { ev=-0.42, d=34, pd=Bh5, mt=00:02:29, tl=01:22:13, s=20927 kN/s, n=3118475249, pv=gxf5 Bh5 Kg8 Bf4 e5 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Rd1 f4 Rd4 Nb5 Rd3 Nbc7 Rd1 h6 Rd3 Ne8 Rd4 Nef6, tb=0, R50=50, wv=0.42, } 22. Bf4 { ev=0.49, d=28, pd=e5, mt=00:07:07, tl=01:12:52, s=22306 kN/s, n=9546958061, pv=Bf4 e5 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Ke7 Bf3 Nef6 Rh4 Kf7 g3 Kg6 Rd4 h5 Kg2 Kg5 Bd1 Kg6 Bb3 Kg5 Bc2, tb=0, R50=49, wv=0.49, } 22... e5 { ev=-0.54, d=36, pd=Bh5, mt=00:01:38, tl=01:21:05, s=19932 kN/s, n=1959582427, pv=e5 Bh5 Kg8 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Rd1 f4 Rd4 Nb5 Rd3 Nbc7 Rd1 h6 Rd4 Nb5 Re4 Nf6 Re5 Nc7, tb=0, R50=50, wv=0.54, } 23. Bh5+ { ev=0.51, d=27, pd=Kg8, mt=00:02:30, tl=01:10:52, s=22206 kN/s, n=3362022616, pv=Bh5 Kg8 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd6 Rxd4 Nb5 Rd1 Nd5 Be2 Kg7 Bc4 Nbc7 g3 Kg6 Kg2 h5 Rd4 Nb5 Rh4 Nf6 Bb3, tb=0, R50=49, wv=0.51, } 23... Kg8 { ev=-0.56, d=36, pd=Rxd4, mt=00:01:26, tl=01:20:10, s=21819 kN/s, n=1880010100, pv=Kg8 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Rd1 f4 Rd4 Nb5 Rd3 Nbc7 Rd1 Kg7 Re1 Kg8 Bf3 h6 Re4 Kg7 Kf1 Kg6, tb=0, R50=49, wv=0.56, } 24. Rxd4 { ev=0.52, d=27, pd=exd4, mt=00:01:29, tl=01:09:53, s=22468 kN/s, n=2034374447, pv=Rxd4 exd4 Bxb8 Rxb8 Rd1 Kg7 Bf3 Nd5 Rxd4 Nef6 Kf1 Kg6 g3 h5 Be2 Ne8 Kg2 Nd6 Bf3 Nb5 Rh4 Nf6 Be2 Nc7 Rd4, tb=0, R50=50, wv=0.52, } 24... exd4 { ev=-0.48, d=36, pd=Bxb8, mt=00:01:48, tl=01:18:52, s=22647 kN/s, n=2451254509, pv=exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Rd1 b5 g3 Rd8 f4 Kf8 Kf2 Rd6 Bf3 Rh6 Nd7 Ke7 Ne5 Rd6 Nd3 Nb6 Re1 Kf6, tb=0, R50=50, wv=0.48, } 25. Bxb8 { ev=0.53, d=26, pd=Rxb8, mt=00:02:38, tl=01:07:46, s=22512 kN/s, n=3582963397, pv=Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Kh2 Kg7 Bf3 Kf7 g3 Kg7 Kg2 Kf7 Rc4 Kg6 g4 Nf6 Be2 fxg4 hxg4 h5 gxh5 Nxh5 Nd7, tb=0, R50=50, wv=0.53, } 25... Rxb8 { ev=-0.52, d=37, pd=Rd1, mt=00:01:34, tl=01:17:49, s=21937 kN/s, n=2060527533, pv=Rxb8 Rd1 Nd5 Rxd4 Nec7 Rd1 b5 g3 Rd8 f4 Kf8 Kf2 Rd6 Bf3 Rh6 h4 Rd6 Nd3 Ke7 Ra1 Nb6 Re1 Kf6 Rc1 Nc4, tb=0, R50=50, wv=0.52, } 26. Rd1 { ev=0.52, d=25, pd=Nd5, mt=00:00:45, tl=01:07:31, s=22461 kN/s, n=1030190815, pv=Rd1 Nd5 Rxd4 Kg7 Bf3 Nec7 Kf1 h6 Rc4 b5 Rd4 Re8 Rd1 Kg6 g3 Kf6 Rd4 Re7 Rh4 Kg7 Kg2 Re1 Nd3 Re7 Rh5, tb=0, R50=49, wv=0.52, } 26... Nd5 { ev=-0.52, d=38, pd=Rxd4, mt=00:01:52, tl=01:16:26, s=21448 kN/s, n=2616184509, pv=Nd5 Rxd4 Nec7 Rd1 b5 g3 Rd8 f4 Kf8 Kf2 Rd6 Bf3 Rh6 h4 Rd6 Nd3 Ke7 Ra1 Nb6 Re1 Kf6 Rc1 Nc4 b3 Nd2, tb=0, R50=49, wv=0.52, } 27. Rxd4 { ev=0.52, d=26, pd=Nec7, mt=00:00:52, tl=01:07:09, s=22317 kN/s, n=1181957570, pv=Rxd4 Nec7 Bf3 Kf7 Kh2 h6 g3 Nf6 Rh4 Kg6 Kg2 h5 Rd4 Ncd5 Rc4 Nc7 Rh4 Nb5 Kf1 Nc7 Rc4 Kg5 Rd4 Nfd5 Kg2, tb=0, R50=50, wv=0.52, } 27... Nec7 { ev=-0.68, d=37, pd=Bf3, mt=00:11:51, tl=01:05:06, s=24031 kN/s, n=17087850847, pv=Nec7 Bf3 Kg7 Kh2 Kf7 g4 Ke7 gxf5 b6 Ne4 Rf8 Rc4 Rxf5 Bg4 Re5 Ng3 Ne6 Rxc6, tb=0, R50=49, wv=0.68, } 28. Bf3 { ev=0.53, d=26, pd=h6, mt=00:01:14, tl=01:06:26, s=22252 kN/s, n=1668022467, pv=Bf3 h6 g3 Kg7 Kg2 Kf7 Rh4 Kg7 Rc4 Kg6 Bd1 Nb5 Bc2 h5 Rh4 Nbc7 Bd3 Ne8 Be2 Nef6 Rd4 Re8 Bd3 Re7 g4, tb=0, R50=49, wv=0.53, } 28... Kf7 { ev=-0.80, d=34, pd=g3, mt=00:02:10, tl=01:03:27, s=22149 kN/s, n=2879316589, pv=Kf7 g3 Kg6 Kg2 Kf7 Kh2 Nf6 Bd1 Ke7 Bc2 Nb5 Rd1 b6 Re1 Kf7 Ne6 Nd5 Bxf5 h6 Nf4 Nd4 Bg6 Kg7 Re4 Nf3, tb=0, R50=48, wv=0.80, } 29. Kh2 { ev=0.53, d=25, pd=h6, mt=00:02:06, tl=01:04:50, s=22272 kN/s, n=2822019119, pv=Kh2 h6 g3 Nf6 Be2 Nfd5 Rh4 Kg6 Kg2 Nb5 Bf3 Nf6 Rf4 Nc7 Rd4 Nfd5 g4 Nb5 Rd1 Nf6 Be2 h5 gxh5 Kh6 Bf3, tb=0, R50=48, wv=0.53, } 29... Nf6 { ev=-0.62, d=35, pd=Rd6, mt=00:02:11, tl=01:01:46, s=21662 kN/s, n=2848166692, pv=Nf6 Rd6 Nce8 Rd3 Nc7 g4 fxg4 hxg4 Ne6 Kg3 Nxc5 bxc5 h6 Kh4 a5 Re3 Rd8 Rb3 Rd7 Bg2 Nd5 Be4 Kf6 f3 Nb4, tb=0, R50=47, wv=0.62, } 30. Bd1 { ev=0.56, d=28, pd=Ncd5, mt=00:03:15, tl=01:02:05, s=18906 kN/s, n=3702145832, pv=Bd1 Ncd5 g4 fxg4 hxg4 b6 Nxa6 Rg8 b5 c5 Rc4 Rg5 b4 cxb4 Nxb4 Nxb4 Rxb4 h5 gxh5 Nxh5 Kh3 Re5 Bf3 Rf5 Bg4, tb=0, R50=47, wv=0.56, } 30... Ncd5 { ev=-0.64, d=34, pd=g3, mt=00:01:29, tl=01:00:47, s=20807 kN/s, n=1860008814, pv=Ncd5 g3 Ke7 Bc2 Kd6 Rd1 Re8 Bxf5 Re2 Kg1 Kc7 Nd3 Ne7 Kf1 Rc2 Be6 Nfd5 Bg4 Kb6 b3 Ka7 Nc5 Rc3 Rd3 Rc1, tb=0, R50=46, wv=0.64, } 31. g4 { ev=0.57, d=28, pd=fxg4, mt=00:04:13, tl=00:58:23, s=22544 kN/s, n=5715181655, pv=g4 fxg4 hxg4 b6 Nxa6 Rg8 b5 c5 Rc4 Rg5 b4 Nxb4 Nxb4 cxb4 Rxb4 h5 gxh5 Nxh5 Kh3 Rc5 Bf3 Rf5 Bg4 Rc5 Bd7, tb=0, R50=50, wv=0.57, } 31... h6 { ev=-0.82, d=35, pd=gxf5, mt=00:01:27, tl=00:59:50, s=20539 kN/s, n=1802062055, pv=h6 gxf5 Ke7 Rh4 h5 Kg3 Rg8 Kf3 b5 Nxa6 Kd6 Nc5 Ke5 Ne6 Rg1 Ke2 Rh1 Bc2 Kd6 Ng5 Rc1 Bd3 Rg1 Ne6 Ra1, tb=0, R50=50, wv=0.82, } 32. gxf5 { ev=0.60, d=25, pd=Ke7, mt=00:05:20, tl=00:53:33, s=3330 kN/s, n=1069159122, pv=gxf5 Ke7 Rh4 h5 Kg3 Rg8 Kf3 Rb8 Bb3 Kd6 Ne4 Ke5 Bxd5 cxd5 Nxf6 Kxf6 Rxh5 Rd8 b3 d4 Ke2 d3 Kd2 b5 Rh6, tb=0, R50=50, wv=0.60, } 32... Ke7 { ev=-0.78, d=37, pd=Rh4, mt=00:01:09, tl=00:59:11, s=20737 kN/s, n=1442876766, pv=Ke7 Rh4 h5 Kg3 Rg8 Kf3 b5 Nxa6 Kd6 Nc5 Rg1 Ke2 Ke5 Ne6 Rh1 Bc2 Kd6 Nd8 Kc7 Nf7 Rc1 Bd3 Rg1 Ne5 Kd6, tb=0, R50=49, wv=0.78, } 33. Kg3 { ev=0.67, d=29, pd=Kd6, mt=00:03:52, tl=00:50:12, s=22573 kN/s, n=5243033029, pv=Kg3 Kd6 Rh4 Rg8 Kf3 b6 Ne4 Ke5 Bc2 Kxf5 Nxf6 Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 Bd3 b5 Be4 Rf7 Bf3, tb=0, R50=49, wv=0.67, } 33... Kd6 { ev=-1.03, d=35, pd=Rh4, mt=00:03:36, tl=00:56:06, s=22382 kN/s, n=4832102826, pv=Kd6 Rh4 Rg8 Kf3 b6 Nxa6 h5 Bb3 Rg5 Bc2 Rg8 b5 c5 b4 Rg1 bxc5 bxc5 Rc4 Nd7 Ke2 Ra1 Be4 Ra2 Kf1 Nf4, tb=0, R50=48, wv=1.03, } 34. Rh4 { ev=0.69, d=27, pd=Rg8, mt=00:00:47, tl=00:49:54, s=22940 kN/s, n=1109422691, pv=Rh4 Rg8 Kf3 b6 Ne4 Ke5 Bc2 Kxf5 Nxf6 Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 f3 b5 Rh6 Kd5 f4 c5 bxc5, tb=0, R50=48, wv=0.69, } 34... Rg8+ { ev=-0.96, d=36, pd=Kf3, mt=00:02:51, tl=00:53:45, s=23639 kN/s, n=4047301070, pv=Rg8 Kf3 b6 Nxa6 h5 Bb3 Rg5 Bc2 Rg8 b5 c5 b4 Rg1 bxc5 bxc5 Rc4 Nd7 Ke2 Ra1 Be4 Ra2 Rc2 Rxc2 Bxc2 c4, tb=0, R50=47, wv=0.96, } 35. Kf3 { ev=0.74, d=27, pd=b6, mt=00:00:43, tl=00:49:42, s=23529 kN/s, n=1016046397, pv=Kf3 b6 Ne4 Ke5 Bc2 Kxf5 Nxf6 Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 f3 Rg8 Kf2 Rc8 Rd4 Ke5 Rd1 Nd5 Re1, tb=0, R50=47, wv=0.74, } 35... b6 { ev=-1.03, d=37, pd=Ne4, mt=00:01:47, tl=00:52:29, s=21995 kN/s, n=2352191797, pv=b6 Ne4 Ke5 Rxh6 Nxe4 Re6 Kxf5 Rxe4 Rh8 h4 Nf6 Rf4 Ke6 Ba4 b5 Bb3 Ke5 Bc2 Ke6 Bg6 Rh6 Bf5 Ke5 Be4 Nd5, tb=0, R50=50, wv=1.03, } 36. Ne4+ { ev=0.72, d=28, pd=Ke5, mt=00:02:02, tl=00:48:10, s=13563 kN/s, n=1672680987, pv=Ne4 Ke5 Bc2 Kxf5 Nxf6 Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 f3 Rg8 Kf2 Rc8 Rf4 Ke5 Rf5 Ke6 Rg5 Rh8 Kg3, tb=0, R50=49, wv=0.72, } 36... Ke5 { ev=-0.98, d=39, pd=Rxh6, mt=00:01:13, tl=00:51:45, s=21817 kN/s, n=1610888716, pv=Ke5 Rxh6 Nxe4 Re6 Kxf5 Rxe4 Rh8 h4 Nf6 Rf4 Ke6 Ba4 b5 Bb3 Ke5 Bc2 Ke6 Bg6 Rh6 Bf5 Ke5 Be4 Nd5 Bxd5 cxd5, tb=0, R50=49, wv=0.98, } 37. Bc2 { ev=0.73, d=28, pd=Kxf5, mt=00:02:24, tl=00:46:16, s=23160 kN/s, n=3337785673, pv=Bc2 Kxf5 Nxf6 Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 f3 Rg8 Kf2 Rc8 Rf4 Ke5 Rf5 Ke6 Rg5 Rh8 f4 Kd6 Kg3, tb=0, R50=48, wv=0.73, } 37... Kxf5 { ev=-0.98, d=38, pd=Rxh6, mt=00:01:04, tl=00:51:11, s=20282 kN/s, n=1307439773, pv=Kxf5 Rxh6 Ke5 Nxf6 Nxf6 Rh4 Kd6 Rc4 Rb8 h4 Rh8 Bg6 b5 Rd4 Ke6 Rf4 Rh6 Bf5 Ke5 Be4 Nd5 Bxd5 cxd5 Ke3 Rh7, tb=0, R50=50, wv=0.98, } 38. Nxf6+ { ev=0.73, d=28, pd=Kxf6, mt=00:01:31, tl=00:45:16, s=19957 kN/s, n=1818843406, pv=Nxf6 Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 Rd4 Ke5 Rc4 Kd6 Kg2 Rg8 Kf3 b5 Rf4 Ke5 h4 Rg1 Rf5 Ke6 Rg5, tb=0, R50=50, wv=0.73, } 38... Kxf6 { ev=-0.98, d=33, pd=Rxh6, mt=00:00:20, tl=00:51:21, s=18200 kN/s, n=377122760, pv=Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Nf6 Rc4 Rb8 h4 Rh8 Bg6 b5 Rd4 Ke6 Rf4 Rh6 Bf5 Ke5 Be4 Nd5 Bxd5 cxd5 Ke3 Rh7, tb=0, R50=50, wv=0.98, } 39. Rxh6+ { ev=0.71, d=26, pd=Ke5, mt=00:00:28, tl=00:45:18, s=23313 kN/s, n=667727965, pv=Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 f3 Ke5 Rc4 Rg8 Kf2 Rd8 f4 Kd5 Bd3 Rh8 Kg3 Rg8 Kh4 b5 Rc3 Kd6 Bf5, tb=0, R50=50, wv=0.71, } 39... Ke5 { ev=-0.98, d=37, pd=Rh5, mt=00:01:06, tl=00:50:45, s=22357 kN/s, n=1480200115, pv=Ke5 Rh5 Kd6 Rh4 Nf6 Rc4 Rb8 h4 Rh8 Bg6 b5 Rd4 Ke6 Rf4 Rh6 Bf5 Ke5 Be4 Nd5 Bxd5 cxd5 Ke3 Rh7 Rg4 Rh8, tb=0, R50=49, wv=0.98, } 40. Rh5+ { ev=0.72, d=27, pd=Kd6, mt=00:00:59, tl=00:44:49, s=23100 kN/s, n=1379492877, pv=Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 f3 Rg8 Kf2 Rb8 Rd4 Ke5 Rc4 Kd6 Be4 c5 Bf5 Kd5 Bd3 cxb4 Rxb4 a5 Rc4 Ke5 f4, tb=0, R50=49, wv=0.72, } 40... Kd6 { ev=-0.98, d=38, pd=Rh4, mt=00:02:17, tl=00:48:58, s=23289 kN/s, n=3202599280, pv=Kd6 Rh4 Nf6 Rc4 Rb8 h4 Rh8 Bg6 b5 Rd4 Ke6 Rf4 Rh6 Bf5 Ke5 Be4 Nd5 Bxd5 cxd5 Ke3 Rh7 Rg4 Rh8 Rd4 Rh5, tb=0, R50=48, wv=0.98, } 41. Rh4 { ev=0.73, d=27, pd=Rf8, mt=00:02:33, tl=00:42:47, s=19449 kN/s, n=2985941436, pv=Rh4 Rf8 Kg3 Nf6 f3 Rg8 Kf2 Ke5 Rc4 Rd8 Kg3 Rg8 Kh2 Kd6 h4 Rh8 Kh3 b5 Rf4 Ke6 Bg6 Rh6 Be4 Rh8 Rf5, tb=0, R50=48, wv=0.73, } 41... Nf6 { ev=-0.98, d=39, pd=Rc4, mt=00:02:39, tl=00:46:49, s=23268 kN/s, n=3712651549, pv=Nf6 Rc4 Rb8 h4 Rh8 Rf4 Ke6 Ba4 b5 Bb3 Ke5 Bc2 Rh6 Rf5 Kd4 Kf4 Nd5 Kg5 Rh8 Rf7 Nxb4 Bg6 Nd5 h5 a5, tb=0, R50=47, wv=0.98, } 42. Rd4+ { ev=0.85, d=26, pd=Ke5, mt=00:01:31, tl=00:41:46, s=23087 kN/s, n=2110176502, pv=Rd4 Ke5 Rc4 Rf8 h4 Nd5 Kg3 Rg8 Kh2 Rf8 Re4 Kd6 Kg3 Nf6 Rd4 Ke5 Rc4 Kd6 Kg2 Nh5 f3 Nf6 Kh3 Kd5 Bb3, tb=0, R50=47, wv=0.85, } 42... Ke5 { ev=-0.98, d=40, pd=Rc4, mt=00:01:18, tl=00:46:02, s=22082 kN/s, n=1721098426, pv=Ke5 Rc4 Kd6 h4 Rh8 Rf4 Ke6 Ba4 b5 Bb3 Ke5 Bc2 Ke6 Rd4 Ke5 Rf4, tb=0, R50=46, wv=0.98, } 43. Rc4 { ev=0.86, d=26, pd=Nd5, mt=00:01:07, tl=00:41:10, s=24141 kN/s, n=1637132054, pv=Rc4 Nd5 Re4 Kd6 Rg4 Rf8 Kg3 Nf6 Rg6 Ke7 Rg7 Ke6 Rg5 Kd6 Bd3 Rh8 f4 Nh5 Kf3 Nf6 Bf5 Nd5 Rg6 Kc7 Rg7, tb=0, R50=46, wv=0.86, } 43... Kd6 { ev=-1.15, d=40, pd=Rf4, mt=00:09:41, tl=00:36:52, s=24077 kN/s, n=14124694124, pv=Kd6 Rf4 Ke7 h4 Rg1, tb=0, R50=45, wv=1.15, } 44. h4 { ev=0.85, d=28, pd=Rh8, mt=00:01:54, tl=00:39:46, s=18868 kN/s, n=2174131272, pv=h4 Rh8 Bg6 Ke7 Bf5 Kd6 Rf4 Ke5 Bg6 Ke6 Bd3 Rd8 Bc2 Ke5 Rc4 Rf8 Kg2 Kd6 Bd3 Nh5 Be2 b5 Rd4 Ke5 Rd2, tb=0, R50=50, wv=0.85, } 44... Rh8 { ev=-0.98, d=41, pd=Rf4, mt=00:00:53, tl=00:36:29, s=21664 kN/s, n=1156190099, pv=Rh8 Rf4 Ke6 Ba4 b5 Bb3 Ke5 Bc2 Ke6 Bg6 Rh6 Bf5 Ke5 Bc2 Kd6 Bb1 Ke6 Ba2 Ke5 Ke3 Rh8 Rd4 Nd5 Bxd5 cxd5, tb=0, R50=49, wv=0.98, } 45. Bf5 { ev=0.85, d=25, pd=Kd5, mt=00:01:47, tl=00:38:29, s=17023 kN/s, n=1838495310, pv=Bf5 Kd5 Bd3 Ke5 Rf4 Ke6 Bg6 Rh6 Bf5 Ke5 Bd3 Rh8 Bc2 Ke6 Bf5 Ke5 Bg6 Ke6 Rc4 Kd5 Bf7 Kd6 Rd4 Ke5 Rf4, tb=0, R50=49, wv=0.85, } 45... Kd5 { ev=-0.98, d=40, pd=Bd3, mt=00:00:58, tl=00:36:01, s=21892 kN/s, n=1271869160, pv=Kd5 Bd3 b5 Rf4 Ke6 Bf5 Ke5 Bc2 Ke6 Bg6 Rh6 Bf5 Ke5 Bc2 Kd6 Bb1 Ke6 Ba2 Ke5 Ke3 Rh8 Rd4 Nd5 Bxd5 cxd5, tb=0, R50=48, wv=0.98, } 46. Bd3 { ev=0.86, d=27, pd=Nd7, mt=00:00:48, tl=00:38:10, s=23871 kN/s, n=1172283690, pv=Bd3 Nd7 Ke3 Nf6 Rd4 Ke6 Bg6 Rh6 Bc2 Rh8 Kf3 Ke5 Rf4 Rd8 Bg6 Rd2 Kg3 Nd5 Rf5 Ke6 h5 Rxb2 h6 Nf6 Rf4, tb=0, R50=48, wv=0.86, } 46... b5 { ev=-1.15, d=42, pd=Rf4, mt=00:08:01, tl=00:28:30, s=24743 kN/s, n=11914412982, pv=b5 Rf4 Ke6 Bf5 Kd6 Bb1 Ke7 Rd4 Ke6 Bc2 Ke5 Rf4 Nd5 Re4 Kf6 Kg2 Rh6 Rd4 Ke6 Kg3 Ke5 Rg4 Kf6 Bb1 Ke5, tb=0, R50=50, wv=1.15, } 47. Rf4 { ev=0.76, d=26, pd=Ke6, mt=00:00:44, tl=00:37:56, s=25491 kN/s, n=1152450075, pv=Rf4 Ke6 Bg6 Ke7 Bf5 Kd6 Bb1 Ke6 Ba2 Ke5 Bb3 Rd8 Bc2 Ke6 Bg6 Rh8 Bb1 Rd8 Ke2 Rh8 Bg6 Nd5 Rd4 Ke5 Re4, tb=0, R50=49, wv=0.76, } 47... Ke6 { ev=-0.98, d=41, pd=Bc2, mt=00:01:11, tl=00:27:49, s=21701 kN/s, n=1568310693, pv=Ke6 Bc2 Rh6 Bb1 Rh8 Ba2 Ke5 Bb1 Ke6, tb=0, R50=49, wv=0.98, } 48. Bg6 { ev=0.77, d=29, pd=Ke7, mt=00:02:44, tl=00:35:42, s=15402 kN/s, n=2540066225, pv=Bg6 Ke7 Ke2 Rh6 Bf5 Kd6 Bc2 Ke5 Kf3 Rh8 Bb1 Ke6 Bf5 Ke5 Bh3 Rh6 Bc8 Kd6 Bf5 Ke5 Bb1 Ke6 Be4 Nd5 Bxd5, tb=0, R50=48, wv=0.77, } 48... Rh6 { ev=-0.98, d=40, pd=Bf5, mt=00:02:37, tl=00:25:42, s=20735 kN/s, n=3269718263, pv=Rh6 Bf5 Ke5 Bb1 Ke6 Ba2 Ke5 Bf7 Kd6 Ba2 Ke5, tb=0, R50=48, wv=0.98, } 49. Bf5+ { ev=0.93, d=26, pd=Kd5, mt=00:01:22, tl=00:34:50, s=24169 kN/s, n=2008523377, pv=Bf5 Kd5 Bb1 Ke6 Kg2 Rh5 Kg3 c5 bxc5 Rxc5 Rf3 Nh5 Kh3 a5 Ba2 Ke5 Bf7 Nf6 Bg6 Ke6 Re3 Kd6 Rd3 Ke7 Rg3, tb=0, R50=47, wv=0.93, } 49... Kd6 { ev=-1.43, d=40, pd=Bb1, mt=00:03:05, tl=00:23:07, s=22303 kN/s, n=4130618123, pv=Kd6 Bb1 Ke7 Ba2 Rh8 Rd4 Rh6 Kg2 Rg6 Kf1 Rh6 f3 Nd5 Re4 Kf6 Kf2 Rh5 Kg3 Rh7 Bb1 Rh8 Bc2 Kf7 Rg4 Kf6, tb=0, R50=47, wv=1.43, } 50. Be4 { ev=0.94, d=26, pd=Kd7, mt=00:02:06, tl=00:33:14, s=20919 kN/s, n=2657794772, pv=Be4 Kd7 Kg2 Nd5 Bf5 Ke7 Rg4 Nf6 Rd4 Rh8 Kg3 Rg8 Kh2 Rh8 Kg2 Rg8 Kf3 Rf8 Bh3 Rf7 Kg3 Rg7 Kf4 Nd5 Kf3, tb=0, R50=46, wv=0.94, } 50... Kd7 { ev=-1.43, d=40, pd=Bb1, mt=00:01:13, tl=00:22:25, s=20710 kN/s, n=1521405869, pv=Kd7 Bb1 Ke7 Ba2 Rh8 Rd4 Rh6 Kg2 Rg6 Kf1 Rh6 f3 Nd5 Re4 Kf6 Kf2 Rh5 Kg3 Rh7 Bb1 Rh8 Bc2 Kf7 Rg4 Kf6, tb=0, R50=46, wv=1.43, } 51. Kg2 { ev=0.97, d=28, pd=Nd5, mt=00:01:12, tl=00:32:32, s=22740 kN/s, n=1646409049, pv=Kg2 Nd5 Bf5 Kd6 Re4 Ne7 Bh3 Nd5 Kg3 Rg6 Rg4 Rh6 Bg2 Ke6 Bf3 Ke5 Bd1 Rd6 Bc2 Nf6 Rg5 Ke6 Bf5 Ke7 Rg7, tb=0, R50=45, wv=0.97, } 51... Kd6 { ev=-1.43, d=35, pd=Bb1, mt=00:00:45, tl=00:22:10, s=20082 kN/s, n=908861798, pv=Kd6 Bb1 Ke7 Kh3 Nd5 Rd4 Kf6 Rg4 Kf7 Kg3 Kf6 f4 Nxb4 f5 Nd5 Rg6 Rxg6 fxg6 Ne7 h5 Kg5 g7 Kh6 Kg4 Kxg7, tb=0, R50=45, wv=1.43, } 52. Bb1 { ev=1.07, d=26, pd=Ke7, mt=00:01:00, tl=00:32:02, s=23657 kN/s, n=1443022225, pv=Bb1 Ke7 Kh3 Rh8 Bg6 Rh6 Be4 Nd5 Rf3 Rh8 Bg6 Nxb4 h5 c5 Kg4 c4 Kg5 Nd5 h6 b4 Rf5 Nc7 Rf7 Kd6 Rf6, tb=0, R50=44, wv=1.07, } 52... Ke7 { ev=-1.51, d=35, pd=Kh3, mt=00:00:47, tl=00:21:53, s=22193 kN/s, n=1045592842, pv=Ke7 Kh3 Nd5 Rg4 Kf6 Kg3 Ke7 Bf5 Kf6 Bc2 Rh8 Bd3 Ke7 Bb1 Nf6 Rg7 Ke6 Rg6 Ke7 f4 Re8 Rg5 Kd6 h5 Re1, tb=0, R50=44, wv=1.51, } 53. Kh3 { ev=1.13, d=26, pd=Rh8, mt=00:02:12, tl=00:30:20, s=23961 kN/s, n=3182939945, pv=Kh3 Rh8 Bg6 Rh6 Be4 Nd5 Rf3 Rh8 Bg6 Nxb4 Kg4 c5 Kg5 Nd5 h5 c4 h6 b4 Rf7 Kd6 Ra7 Nc7 h7 c3 bxc3, tb=0, R50=43, wv=1.13, } 53... Nd5 { ev=-1.73, d=35, pd=Rg4, mt=00:02:24, tl=00:19:59, s=23528 kN/s, n=3401044408, pv=Nd5 Rg4 Kf7 Kg3 Ke7 Bc2 Kf6 Bd3 Ke7 Bc2, tb=0, R50=43, wv=1.73, } 54. Rf3 { ev=1.13, d=24, pd=Nxb4, mt=00:01:15, tl=00:29:36, s=24526 kN/s, n=1852644326, pv=Rf3 Nxb4 Kg4 Rf6 Bf5 Nd5 Kg5 Kf8 h5 Kg7 Be4 Rxf3 Bxf3 a5 h6 Kg8 Bg4 a4 Be6 Kh7 Bf5 Kh8 Bc2 Nb6 f4, tb=0, R50=42, wv=1.13, } 54... Nxb4 { ev=-2.10, d=32, pd=Kg4, mt=00:00:41, tl=00:19:48, s=17527 kN/s, n=730893451, pv=Nxb4 Kg4 Rf6 Bf5 Nd5 Kg5 Kf8 Ra3 Kf7 h5 Ne7 Bg4 Rd6 Bf3 Rf6 Be4 Re6 f3 Re5 Kh4 Nf5 Kg4 Nh6 Kg3 Ke7, tb=0, R50=50, wv=2.10, } 55. Kg4 { ev=1.44, d=24, pd=Rf6, mt=00:00:22, tl=00:29:44, s=25114 kN/s, n=572786733, pv=Kg4 Rf6 Bf5 a5 h5 Nd5 Kg5 a4 h6 Kf8 Rd3 Rd6 f4 Kg8 Rd4 Rd8 Be6 Kh7 f5 Re8 Bxd5 cxd5 Rxd5 Rg8 Kh5, tb=0, R50=49, wv=1.44, } 55... Rf6 { ev=-2.34, d=35, pd=Bf5, mt=00:00:55, tl=00:19:23, s=19594 kN/s, n=1090977729, pv=Rf6 Bf5 Nd5 Kg5 Kf8 Ra3 Kg7 Rxa6 Ne7 Be4 Re6 f3 Re5 Kg4 Rc5 h5 Ng8 Ra7 Kf6 b4 Nh6 Kf4 Rxh5 Ra6 Nf7, tb=0, R50=49, wv=2.34, } 56. Bf5 { ev=1.26, d=24, pd=Kf8, mt=00:00:28, tl=00:29:46, s=25965 kN/s, n=750983835, pv=Bf5 Kf8 Kg5 Nd5 h5 Kg7 Rd3 a5 f4 Rf8 Rd4 a4 Bg6 Kh8 h6 b4 f5 Ra8 f6 Nxf6 Kxf6 c5 Rd1 a3 bxa3, tb=0, R50=48, wv=1.26, } 56... Nd5 { ev=-2.62, d=37, pd=Kg5, mt=00:01:00, tl=00:18:53, s=19435 kN/s, n=1167922719, pv=Nd5 Kg5 Kf8 Ra3 Kg7 Rxa6 Ne7 Be4 Re6 f3 Re5 Kg4 Rc5 h5 Ng8 h6 Nxh6 Kf4 b4 Bxc6 Ng8 Rb6 Ne7 Be4 Nd5, tb=0, R50=48, wv=2.62, } 57. h5 { ev=1.14, d=25, pd=Kf8, mt=00:00:55, tl=00:29:22, s=25280 kN/s, n=1402815659, pv=h5 Kf8 Kg5 Kg7 Rd3 a5 Rd4 a4 f4 b4 Be4 Rd6 Bg6 Re6 h6 Kh8 Bf7 Re2 Bxd5 cxd5 Rxb4 Rg2 Kf6 Kh7 f5, tb=0, R50=50, wv=1.14, } 57... Kf7 { ev=-2.12, d=36, pd=Kg5, mt=00:00:39, tl=00:18:44, s=16797 kN/s, n=667163528, pv=Kf7 Kg5 Kg7 Ra3 Ne7 Bg4 Ng8 f4 Nh6 Bc8 Nf7 Kg4 Rd6 Rxa6 Kf6 Ra7 Nh6 Kf3 Nf5 Rc7 Rd3 Ke2 Rd6 Bd7 b4, tb=0, R50=49, wv=2.12, } 58. Kg5 { ev=1.21, d=26, pd=Kg7, mt=00:00:34, tl=00:29:18, s=26224 kN/s, n=918450737, pv=Kg5 Kg7 Rd3 a5 f4 a4 Rd4 Rf8 Bg6 Kh8 h6 b4 f5 Ra8 f6 Nxf6 Kxf6 a3 Rd1 a2 Ra1 b3 Be4 Re8 Bf5, tb=0, R50=49, wv=1.21, } 58... Kg7 { ev=-2.36, d=39, pd=Rd3, mt=00:02:18, tl=00:16:56, s=20001 kN/s, n=2778051940, pv=Kg7 Rd3 a5 f4 Rf8 Rd4 a4 h6 Kh8 Be6 Kh7 Re4 Nf6 Re5 b4 f5 b3 Ra5 Ne4 Kf4 Nf6 Rxa4 Kxh6 Ra6 Rd8, tb=0, R50=48, wv=2.36, } 59. Rd3 { ev=1.46, d=27, pd=a5, mt=00:01:44, tl=00:28:04, s=26189 kN/s, n=2747152311, pv=Rd3 a5, tb=0, R50=48, wv=1.46, } 59... a5 { ev=-2.44, d=39, pd=f4, mt=00:00:46, tl=00:16:40, s=20111 kN/s, n=936862769, pv=a5 f4 Rf8 Rd4 a4 Bd7 Kf7 Bxc6 Rg8 Kh4 Ne3 Bd7 Rg1 Rb4 Rh1 Kg3 Rxh5 Bxb5 Kf6 Bxa4 Nf5 Kg2 Nd6 Bd1 Rc5, tb=0, R50=50, wv=2.44, } 60. f4 { ev=1.50, d=25, pd=a4, mt=00:00:38, tl=00:27:56, s=25525 kN/s, n=1004264929, pv=f4 a4 Rd4 Rf8 Bg6 Kg8 Re4 b4 f5 Kh8 Rc4 Ra8 Rxc6 a3 bxa3 bxa3 Bf7 Ne7 Rh6 Kg7 Ba2 Nxf5 Rg6 Kh7 Kxf5, tb=0, R50=50, wv=1.50, } 60... a4 { ev=-2.56, d=39, pd=Rd4, mt=00:01:02, tl=00:16:08, s=19746 kN/s, n=1236202016, pv=a4 Rd4 Rf8 Bd7 Kh8 Bxc6 Rg8 Kh4 Ne3 Kh3 Rb8 Re4 Nc4 Re2 Nd6 Kg4 Rg8 Kf3 Rc8 Bd7 Rc7 Be6 Kg7 Re5 b4, tb=0, R50=50, wv=2.56, } 61. Rd4 { ev=1.60, d=26, pd=b4, mt=00:00:45, tl=00:27:40, s=25749 kN/s, n=1193840721, pv=Rd4 b4 Be4 Rxf4 h6 Kh8 Rxb4 Rf8 Rxa4 Rg8 Kf5 Re8 Ra3 Kh7 Ra6 Kh8 Ra7 Ne3 Kf4 Nd5 Kf3 Rf8 Kg3 Ne3 Bg6, tb=0, R50=49, wv=1.60, } 61... b4 { ev=-3.05, d=39, pd=Be4, mt=00:03:55, tl=00:12:43, s=21422 kN/s, n=5038948625, pv=b4 Be4 Rxf4 h6 Kh8 Rxb4 Rf8 Rxa4 Rg8 Kh4 Ne3 Bb1 c5 Kh5 Nd5 Bd3 Rd8 Bg6 Rg8 Bb1 Nf6 Kh4 Rb8 Ra2 Kg8, tb=0, R50=50, wv=3.05, } 62. Be4 { ev=1.60, d=28, pd=Rxf4, mt=00:00:53, tl=00:27:18, s=25164 kN/s, n=1345369401, pv=Be4 Rxf4 h6 Kh8 Rxb4 Rf8 Rxa4 Rg8 Kf5 Re8 Ra3 Kh7 Ra6 Kh8 Ra7 Ne3 Kf4 Nd5 Kf3 Rf8 Ke2 Nf6 Bxc6 Ng4 Ra8, tb=0, R50=49, wv=1.60, } 62... Rxf4 { ev=-3.23, d=38, pd=h6, mt=00:00:40, tl=00:12:33, s=19599 kN/s, n=788859784, pv=Rxf4 h6 Kh8 Rxb4 Rf8 Rxa4 Rg8 Kf5 Re8 Ra7 Ne3 Kf4 Nd5 Kf3 Rd8 Bg6 c5 Rh7 Kg8 Rb7 Kh8 Ke4 c4 Ke5 Ne3, tb=0, R50=50, wv=3.23, } 63. h6+ { ev=1.68, d=26, pd=Kh8, mt=00:00:24, tl=00:27:24, s=27153 kN/s, n=684938612, pv=h6 Kh8 Rxb4 Rf8 Rxa4 Rg8 Kf5 Re8 Ra7 Ne3 Kf4 Nd5 Kf3 Rd8 Bg6 c5 Rh7 Kg8 Rg7 Kh8 Ke4 c4 Rh7 Kg8 Rb7, tb=0, R50=50, wv=1.68, } 63... Kh8 { ev=-3.39, d=39, pd=Rxb4, mt=00:00:52, tl=00:12:12, s=18897 kN/s, n=991435870, pv=Kh8 Rxb4 Rf8 Rxa4 Rg8 Kf5 Re8 Ra3 Kh7 Ra6 Kh8 Bb1 Ne7 Kg5 Rg8 Kh5 Re8 Ra7 Nd5 Bf5 Ne7 Bd3 Nd5 Kg5 Rf8, tb=0, R50=49, wv=3.39, } 64. Rxb4 { ev=1.66, d=26, pd=Rf8, mt=00:00:27, tl=00:27:27, s=28204 kN/s, n=784938013, pv=Rxb4 Rf8 Rxa4 Rg8 Kf5 Re8 Ra7 Ne3 Kf4 Nd5 Kf3 Rf8 Kg3 Ne3 Bg6 Nc4 Rh7 Kg8 Rb7 c5 Kh4 Rd8 Kg5 Rd5 Kf4, tb=0, R50=50, wv=1.66, } 64... Rf8 { ev=-3.39, d=41, pd=Rxa4, mt=00:00:34, tl=00:12:07, s=16725 kN/s, n=582664321, pv=Rf8 Rxa4 Rg8 Kf5 Re8 Ra3 Kh7 Ra6 Kh8 Bb1 Ne7 Kg5 Rg8 Kh5 Rb8 Bc2 Rb5 Kg4 Rb4 Kg3 Rb8 Ra7 Nd5 Kh4 Ne3, tb=0, R50=49, wv=3.39, } 65. Rxa4 { ev=1.66, d=24, pd=Rg8, mt=00:00:19, tl=00:27:38, s=27502 kN/s, n=550464666, pv=Rxa4 Rg8 Kf5 Re8 Ra7 Ne3 Kf4 Nd5 Kf3 Rf8 Kg3 Ne3 Bg6 Nc4 Rh7 Kg8 Rb7 c5 Kh4 Ne5 Bh7 Kh8 Kh5 c4 Be4, tb=0, R50=50, wv=1.66, } 65... Rg8+ { ev=-4.62, d=42, pd=Kf5, mt=00:04:28, tl=00:08:10, s=18405 kN/s, n=5006201437, pv=Rg8 Kf5 Nc7 Ke5 Rb8 Kd6 Ne8 Kxc6 Rd8 b4 Rd4 Bf5 Rd6 Kc5 Rf6 Be4 Nc7 b5 Ne6 Kd6 Rxh6 Ra6 Ng5 Ke5 Nf7, tb=0, R50=49, wv=4.62, } 66. Kf5 { ev=1.66, d=24, pd=Re8, mt=00:00:18, tl=00:27:50, s=27831 kN/s, n=521453887, pv=Kf5 Re8 Ra7 Ne3 Kf4 Nd5 Kf3 Rd8 Rh7 Kg8 Rg7 Kh8 Rf7 Kg8 Bg6 c5 Ke4 c4 Rb7 Kh8 Ke5 Ne3 Rh7 Kg8 Rg7, tb=0, R50=49, wv=1.66, } 66... Nb6 { ev=-4.24, d=39, pd=Ra7, mt=00:00:46, tl=00:07:54, s=13286 kN/s, n=620770403, pv=Nb6 Ra7 Rd8 Kg6 Nd5 Bf5 Nf4 Kf6 Nd5 Kg5 Rg8 Bg6 Rd8 Rh7 Kg8 Bb1 Ra8 Rg7 Kh8 Rd7 Rb8 Be4 Ra8 Rb7 Rg8, tb=0, R50=48, wv=4.24, } 67. Ra7 { ev=1.82, d=25, pd=Rf8, mt=00:00:56, tl=00:27:25, s=27386 kN/s, n=1540626068, pv=Ra7 Rf8 Kg6 Nd5 Kg5 Rg8 Bg6 Rb8 Rh7 Kg8 Rg7 Kh8 Ra7 Ne3 Rh7 Kg8 Rg7 Kh8 Be4 Nd5 Rf7 Kg8 Bg6 Ne3 Rf6, tb=0, R50=48, wv=1.82, } 67... Rd8 { ev=-5.49, d=39, pd=Kg6, mt=00:00:27, tl=00:07:57, s=14280 kN/s, n=386981247, pv=Rd8 Kg6 Nd5 Bf5 Nf4 Kg5 Ne2 b4 Nd4 Be4 Re8 Bb1 Rg8 Kf6 Rd8 Be4 Ne2 Rh7 Kg8 Rb7 Rf8 Kg5 Nd4 Rg7 Kh8, tb=0, R50=47, wv=5.49, } 68. Kg6 { ev=2.37, d=26, pd=Nd5, mt=00:01:07, tl=00:26:48, s=25914 kN/s, n=1754924098, pv=Kg6 Nd5, tb=0, R50=47, wv=2.37, } 68... Nd5 { ev=-6.26, d=41, pd=Bf5, mt=00:00:57, tl=00:07:30, s=13711 kN/s, n=784625644, pv=Nd5 Bf5 Nf4 Kg5 Nd5 Rh7 Kg8 Rg7 Kh8 Bd7 c5 Bf5 Ne3 Be6 Rd1 Re7 Rg1 Kf4 Nc2 Re8 Kh7 Bf5 Kxh6 Bxc2 Rf1, tb=0, R50=46, wv=6.26, } 69. Bf5 { ev=2.38, d=27, pd=Rd6, mt=00:01:26, tl=00:25:51, s=21903 kN/s, n=1908353467, pv=Bf5 Rd6 Kg5 Rd8 Rh7 Kg8 Rd7 Rf8 b4 Kh8 b5 Nf6 Rd6 cxb5 Rxf6 Rxf6 Kxf6 b4 Kg5 b3 Kg6 Kg8 Kf6 b2 Bb1, tb=0, R50=46, wv=2.38, } 69... Nf4+ { ev=-6.62, d=42, pd=Kg5, mt=00:00:35, tl=00:07:26, s=11504 kN/s, n=403827329, pv=Nf4 Kg5 Nd5 Rh7 Kg8 Rg7 Kh8 Bd7 c5 Bf5 Ne3 Be6 Rd1 Re7 Rg1 Kf4 Nc2 Re8 Kh7 Bf5 Kxh6 Bxc2 Rf1 Kg3 Kg7, tb=0, R50=45, wv=6.62, } 70. Kg5 { ev=2.32, d=25, mt=00:00:31, tl=00:25:50, s=32345 kN/s, n=1027845318, pv=Kg5, tb=0, R50=45, wv=2.32, } 70... Ne2 { ev=-6.82, d=44, pd=b4, mt=00:00:43, tl=00:07:13, s=12458 kN/s, n=537381739, pv=Ne2 b4 Nd4 Be4 Re8 Bg6 Rf8 Rh7 Kg8 Re7 Nf3 Kg4 Nd4 Bh7 Kh8 Be4 Kg8 Kg5 Rd8 Kf6 Rd6 Ke5 Rd8 Kf6, tb=0, R50=44, wv=6.82, } 71. Bb1 { ev=2.80, d=25, pd=Nd4, mt=00:00:53, tl=00:25:27, s=30061 kN/s, n=1625883663, pv=Bb1 Nd4 Kg6 Rd6 Kf7 Rd8 Rc7 Rb8 b4 Rd8 Kf6 Kg8 Rg7 Kh8 Ke7 Rd5 Rf7 Re5 Kd8 Rd5 Rd7 Ne6 Ke7 Nf4 Rc7, tb=0, R50=44, wv=2.80, } 71... Rg8+ { ev=-6.82, d=43, pd=Bg6, mt=00:00:59, tl=00:06:45, s=11718 kN/s, n=694850138, pv=Rg8 Bg6 Rd8 Rh7 Kg8 Re7 Nd4 b4 Kf8 Rf7 Kg8 Rg7 Kh8 Re7 Nf3 Kg4 Nd4 Be4 Rf8 Kg5 Rd8 Kg6 Kg8 Rg7 Kf8, tb=0, R50=43, wv=6.82, } 72. Kf6 { ev=3.29, d=23, pd=Rf8, mt=00:00:20, tl=00:25:38, s=31085 kN/s, n=636227816, pv=Kf6 Rf8 Rf7 Rxf7 Kxf7 Nf4 Kf6 Nd5 Ke5 Nb6 Bd3 Na4 b4 Nc3 Kd6 Nd5 Kc5 Nc3 Bf5 Na2 Bg6 Nxb4 Kxb4 c5 Kc4, tb=0, R50=43, wv=3.29, } 72... Rb8 { ev=-6.82, d=42, pd=Rh7, mt=00:00:28, tl=00:06:47, s=10625 kN/s, n=305491585, pv=Rb8 Rh7 Kg8 Rc7 Rf8 Kg5 Rd8 Rg7 Kh8 Rh7 Kg8 Re7 Nd4 b4 Nf3 Kg6 Rd6 Kh5 Rd8 Be4 Nd4 Kg5 Rf8 Kg6 Rd8, tb=0, R50=42, wv=6.82, } 73. Rh7+ { ev=3.46, d=25, pd=Kg8, mt=00:00:42, tl=00:25:26, s=29635 kN/s, n=1263035475, pv=Rh7 Kg8, tb=0, R50=42, wv=3.46, } 73... Kg8 { ev=-6.82, d=12, pd=Rc7, mt=00:00:00, tl=00:07:17, s=948 kN/s, n=948, pv=Kg8 Rc7 Rf8 Kg5 Rd8 Rg7 Kh8 Rh7 Kg8 Re7 Nd4 b4 Nf3 Kg6 Rd6 Kh5 Rd8 Be4 Nd4 Kg5 Rf8 Kg6 Rd8 Rg7 Kh8, tb=0, R50=41, wv=6.82, } 74. Rc7 { ev=3.92, d=24, pd=Rf8, mt=00:00:20, tl=00:25:37, s=30953 kN/s, n=636424900, pv=Rc7 Rf8 Kg5 Rd8 Rxc6 Nd4 Rf6 Kh8 b4 Rg8 Kh5 Rd8 Bd3 Nb3 Kg6 Rg8 Kf7 Nc1 Rg6 Rd8 Bf5 Nd3 Ke7 Rd5 Bxd3, tb=0, R50=41, wv=3.92, } 74... Rf8+ { ev=-8.54, d=42, pd=Kg5, mt=00:01:30, tl=00:06:17, s=14704 kN/s, n=1328868368, pv=Rf8 Kg5 Rd8 Rxc6 Nd4 Rg6 Kh8 Rb6 Kg8 Bg6 Ne2 Re6 Nd4 Re8 Rxe8 Bxe8 Ne6 Kf6 Nd4 Bg6 Nc6 Be4 Nb4 Ke6 Na6, tb=0, R50=40, wv=8.54, } 75. Kg5 { ev=4.08, d=24, pd=Rd8, mt=00:00:15, tl=00:25:52, s=31568 kN/s, n=481137862, pv=Kg5 Rd8 Rxc6 Nd4 Rf6 Ne2 b4 Kh8 Bf5 Ng3 Bg6 Kg8 Re6 Rb8 Re5 Nf1 Re8 Rxe8 Bxe8 Ne3 b5 Nd5 Bd7 Nb6 Bc6, tb=0, R50=40, wv=4.08, } 75... Rd8 { ev=-10.62, d=43, pd=Rxc6, mt=00:01:28, tl=00:05:19, s=14609 kN/s, n=1285922662, pv=Rd8 Rxc6 Nd4 Rg6 Kh8 Rb6 Rc8 Kf6 Kg8 Bg6 Rf8 Kg5 Rd8 b4 Kh8 Kf6 Ra8 Rd6 Nb5 Rd7 Rf8 Kg5 Na3 Be4 Nc4, tb=0, R50=39, wv=10.62, } 76. Rxc6 { ev=4.08, d=29, pd=Nd4, mt=00:00:27, tl=00:25:55, s=30546 kN/s, n=864429078, pv=Rxc6 Nd4 Rf6 Rb8 Kg6 Kh8 b4 Rg8 Kh5 Rd8 Bd3 Nb3 Kg6 Rg8 Kf7 Nd2 b5 Rd8 Kg6 Rg8 Kh5 Rd8 Bf5 Nc4 b6, tb=0, R50=50, wv=4.08, } 76... Nd4 { ev=-14.48, d=43, pd=Rg6, mt=00:02:16, tl=00:03:34, s=13591 kN/s, n=1847437481, pv=Nd4 Rg6 Kh8 Rb6 Ra8 Bg6 Rf8 b4 Nf3 Kh5 Rd8 Re6 Nd4 Re8 Rxe8 Bxe8 Ne6 Bg6 Nc7 Bf7 Nb5 Kg5 Nd6 Bg6 Nb5, tb=0, R50=49, wv=14.48, } 77. Rg6+ { ev=4.84, d=32, pd=Kh8, mt=00:02:41, tl=00:23:44, s=31007 kN/s, n=5030257930, pv=Rg6 Kh8 Rb6 Rf8 b4 Nf3 Kg6 Nd2 Ba2 Nf3 b5 Ne5 Kg5 Nf3 Kh5 Rf5 Kg4 Rf8 Bb1 Nd2 Bf5 Nf1 Bg6 Ne3 Kg5, tb=0, R50=49, wv=4.84, } 77... Kh8 { ev=-14.48, d=45, pd=Rb6, mt=00:00:10, tl=00:03:55, s=9592 kN/s, n=102359746, pv=Kh8 Rb6 Ra8 Bg6 Rf8 b4 Nf3 Kh5 Rd8 Re6 Nd4 Re8 Rxe8 Bxe8 Ne6 Bg6 Nc7 Bf7 Nb5 Kg5 Nd6 Bg6 Nb5 Kf5 Nd4, tb=0, R50=48, wv=14.48, } 78. Rb6 { ev=5.10, d=29, pd=Nf3, mt=00:00:32, tl=00:23:42, s=31601 kN/s, n=1036245727, pv=Rb6 Nf3 Kf6 Nd2 Ke7 Ra8 Bd3 Rc8 b4 Nc4 Rb7 Ne3 b5 Nd5 Kf7 Rg8 Bg6 Ne3 Kf6 Rf8 Ke7 Rg8 Be4 Nc4 Kf6, tb=0, R50=48, wv=5.10, } 78... Ra8 { ev=-18.42, d=46, pd=Rd6, mt=00:01:43, tl=00:02:42, s=12109 kN/s, n=1252881054, pv=Ra8 Rd6 Rg8 Bg6 Nb3 Rd7 Ra8 Bf5 Rg8 Kf6 Ra8 Rh7 Kg8 Rg7 Kh8 Rd7 Rb8 Ke7 Nc5 Rd8 Rxd8 Kxd8 Na4 b4 Nc3, tb=0, R50=47, wv=18.42, } 79. b4 { ev=6.10, d=28, pd=Nf3, mt=00:00:42, tl=00:23:30, s=32164 kN/s, n=1385855432, pv=b4 Nf3 Kf6 Nd2 Bg6 Rg8 b5 Nc4 Rb7 Rd8 Ke7 Rg8 Bf5 Ne3 Be4 Rc8 Rd7 Nc4 Rd8 Rxd8 Kxd8 Na5 b6 Kg8 Ke7, tb=0, R50=50, wv=6.10, } 79... Kg8 { ev=-18.52, d=33, pd=Rd6, mt=00:00:09, tl=00:03:03, s=6124 kN/s, n=56763876, pv=Kg8 Rd6 Nf3 Kh5 Re8 Rd7 Rb8 Bf5 Ne5 Rc7 Nf3 Be4 Nd4 Rg7 Kh8 Rh7 Kg8 Rd7 Ne2 Kg5 Nc3 Bh7 Kh8 Bf5 Nb5, tb=0, R50=49, wv=18.52, } 80. Rb7 { ev=7.40, d=24, pd=Nf3, mt=00:00:24, tl=00:23:37, s=31904 kN/s, n=786378088, pv=Rb7 Nf3 Kh5 Nd4 b5 Ne6 Bf5 Nd4 Be4 Rd8 b6 Ne6 Re7 Nc5 Bf5 Kh8 Rh7 Kg8 Rg7 Kh8 b7 Nxb7 Rxb7 Rf8 Kg5, tb=0, R50=49, wv=7.40, } 80... Ne6+ { ev=-76.96, d=35, pd=Kf6, mt=00:00:25, tl=00:03:08, s=7017 kN/s, n=181522471, pv=Ne6 Kf6 Nd4 b5 Rd8 Ba2 Kh8 Kg5 Nf3 Kh5 Re8 Bb1 Nd4 b6 Re5 Kg4 Re8 Bg6 Rf8 Rh7 Kg8 Rc7 Nb5 Rg7 Kh8, tb=0, R50=48, wv=76.96, } 81. Kf6 { ev=9.23, d=24, pd=Nd8, mt=00:00:20, tl=00:23:47, s=34878 kN/s, n=729105782, pv=Kf6 Nd8 Rb6 Ra1 Bf5 Ra8 Ke7 Kh8 Bg6 Kg8 Be4 Rc8 Rd6 Nb7 Bxb7 Rc7 Rd7 Rxd7 Kxd7 Kh7 Be4 Kxh6 Ke7 Kg5 b5, tb=0, R50=48, wv=9.23, } 81... Nf8 { ev=-133.68, d=38, pd=b5, mt=00:01:55, tl=00:01:42, s=10792 kN/s, n=3001098897, pv=Nf8 b5 Rd8 Ba2 Kh8 b6 Rd6 Ke7 Rxh6 Kxf8 Rc6 Rb8 Kh7 Ke7 Kg6 Be6 Kg5 b7 Rb6 Rg8 Kf4 b8=Q Rxb8 Rxb8 Ke3, tb=0, R50=47, wv=133.68, } 82. b5 { ev=#21, d=24, pd=Rd8, mt=00:00:52, tl=00:23:25, s=30626 kN/s, n=1604808840, pv=b5 Rd8 Rg7 Kh8 b6 Nd7 Kg6 Ne5 Kh5 Nc6 Rc7 Na5 b7 Rd5 Kg6 Rd6 Kg5 Rd8 Bg6 Nxb7 Rxb7 Rd5 Bf5 Rd8 Rh7, tb=0, R50=50, wv=#21, } 82... Rd8 { ev=-135.90, d=37, pd=Ba2, mt=00:01:13, tl=00:00:59, s=7185 kN/s, n=2277060548, pv=Rd8 Ba2 Kh8 b6 Rd2 Rb8 Rf2 Ke7 Re2 Kxf8 Rxa2 Rc8 Rb2 Ke7 Kh7 Rc6 Kg8 Kd6 Kf7 Kc7 Re2 h7 Re7 Kb8 Kg7, tb=0, R50=49, wv=135.90, } 83. Rg7+ { ev=#20, d=22, pd=Kh8, mt=00:00:12, tl=00:23:43, s=22775 kN/s, n=287789565, pv=Rg7 Kh8 b6 Nd7 Kg6 Ne5 Kh5 Nc6 Rh7 Kg8 Rc7 Rd5 Kh4 Rd1 Bh7 Kf8 Be4 Ne7 h7 Kf7 h8=Q Ke6 Rxe7 Kxe7 b7, tb=0, R50=49, wv=#20, } 1-0
  """.trim

  val output2 =
    """
[Event "FIDE World Championship 2023"]
[Site "Astana, Kazakhstan"]
[Date "2023.04.09"]
[Round "1.1"]
[White "Nepomniachtchi, Ian"]
[Black "Liren, Ding"]
[Result "1/2-1/2"]
[WhiteElo "2795"]
[BlackElo "2788"]
[Annotator "Navara,David"]
[UTCDate "2023.04.11"]
[UTCTime "07:41:19"]
[Variant "Standard"]
[ECO "C85"]
[Opening "Ruy Lopez: Closed, Delayed Exchange"]

{ The initial game of a World Championship match is always a bit specific. Both players have come there very well prepared, but they know little about the opponent's preparation. Is the opponent willing to enter a theoretical debate, repeating the same openings again or again, or is he going to vary them, coming with many surprising lines for one or two games? In the initial games
the players are also trying to learn as much as possible about the opponent's preparation, while trying not to reveal much about their own. It makes sense to surprise the opponent, but one should not take too many risks, as a loss in a relatively short match might cause a player big problems. }
1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 { Contrary to many other top players, Ding Liren mostly plays 3...a6, although he sometimes plays Berlin, too. Ian Nepomniachtchi has undoubtedly prepared some dangerous ideas against it. } 4. Ba4 Nf6 5. O-O Be7 6. Bxc6 { This move probably came as a surprise. As far as I know, Ian
Nepomniachtchi had not played this move before. White steers the game into a less explored position with some imbalances. Black's doubled pawn is not much of a problem. On the other hand, his bishop pair is not so strong so far. Why
White did not take on c6 already on move 4? Black's e5-pawn might hang in many lines, and Black will need to spend the extra tempo on protecting it. Objectively speaking, White has no advantage here, but it is the case with most topical lines nowadays. The element of surprise can be quite important, as Ian Nepomniachtchi has surely studied the resulting positions deeper. } (6. Re1 { and }) (6. d3 { are much more popular and have also been played by Ian Nepomniachtchi many times. }) 6... dxc6 7. Re1!? { For the second time in a row, Ian Nepomniachtchi chooses a less common move. I am not sure if Ding Liren faced this move before. } (7. d3 { is much more common. Then } 7... Nd7 { leads to quieter positions with mutual chances. Ding Liren faced 7.d3 at least three times in recent years. }) 7... Nd7 { Ding spent 9 minutes on this move. When going for } (7... Bg4 { , Black has to be ready for } 8. h3 Bh5 9. g4!? { . } (9. d3 { is a reasonable positional move, but it looks less dangerous for both
sides, primarily for the one which had not expected this line. }) 9... Nxg4 (9... Bg6 10. Nxe5 { also deserves a deeper analysis, Black has various option
there. }) 10. hxg4 Bxg4 { looks fine for Black, but it is mostly a good idea to avoid the sharpest lines when a well-prepared opponent surprises you. And there is no doubt that both players came very well prepared! A very sharp position with mutual chances could arise after } 11. d4!? exd4 12. Qd3 c5 13. Nbd2 Qd6 14. c3 { . }) 8. d4!? { White opens the position, postponing his queenside development for the time being. After } (8. Nc3 { Black has reasonable ways to avoid the exchange on d4, e.g. } 8... c5 { or } (8... O-O 9. d4 Bd6!? { , with approximate equality in both cases. })) 8... exd4 { Here } (8... Bd6?! { is less good in view of } 9. Nbd2! exd4 10. Nc4!? $14 { or } (10. Nxd4 { , as Black's bishop on d6 becomes a target. (The exchange for a White's knight is mostly undesirable, as it deprives Black of his main asset, the bishop pair. })) 9. Qxd4 O-O 10. Bf4 { Black need not be afraid of } (10. Nc3 Bf6 11. Qd3 (11. e5? { is a natural but careless move which runs into nice tactics: } 11... Nxe5! 12. Qxd8 (12. Rxe5? Qxd4 13. Nxd4 Bxe5 $19) 12... Nxf3+ 13. gxf3 Bxd8 { , when Black remains a healthy pawn up. }) 11... Ne5 { Black can exchange some pieces and develop his queenside with decent play. }) 10... Nc5 11. Qe3 (11. Qxd8?! Bxd8 { leads to an endgame which is good for Black. White's pawn
majority is not very relevant with so many pieces on the board, he would need to exchange the dark-squared bishops and rooks to make his kingside majority count. Without losing his queenside pawns, of course. As it is, Black can just develop the pieces and achieve a good position. }) 11... Bg4?! { This move might be inaccurate. It is not so easy to handle an unfamiliar position over the board. After } (11... Ne6! 12. Bg3 { Black has many reasonable moves leading to approximate equality. Personally I like } (12. Nc3?! Nxf4 13. Qxf4 Qd6 14. e5 Qg6 $15 { looks promising for Black, whose bishop pair might become more important in the future. }) 12... Bc5 { as well as } (12... f6 { , as these two moves give some room to Black's queen, who might feel uncomfortable on the open d-file. })) 12. Nd4 { After } (12. h3?! Bxf3! 13. Qxf3 Qd4 14. Nc3 Ne6 15. Be3 Qb4 { Black's activity fully compensates for a minor structure defect. In
fact, the c6-pawn protects the d5-square. }) 12... Qd7 { While } (12... Bd6?! 13. Bxd6 cxd6 { would have undoubled Black's pawns, the d6-pawn would remain weak. White would have a better development and a slight advantage after } 14. f3 Be6 15. Nc3 $14 { or even } (15. Nd2!? Re8 16. Rad1 Qc7 17. Nf1!? $14 { . })) 13. Nc3 { Ian Nepomniachtchi played very quickly until now, but spent 24 minutes on this move. Either he was already on his own, or he had forgotten the details of his preparation. There is a plenty of theory, so it is impossible to prepare/remember everything. Harikrishna Pentala found a nice idea during his
commentary, namely } (13. h3! Rad8 14. Nc3! Qxd4? (14... Bf6 15. Nf3 Bxf3 16. Qxc5! b6 17. Qf5 Qxf5 18. exf5 Bxc3 19. bxc3 Bd5 20. Bxc7 $16 { also
leaves White with a clear advantage. }) 15. Nd5! { , when White wins material: } 15... Rxd5 (15... Qxe3 16. Nxe7+ Kh8 17. Bxe3 Nxe4 18. hxg4 $18) 16. exd5 Qxe3 17. Rxe3 $18 { . We will learn more about this option from the next note. }) 13... Rad8 14. Nf5?! { White could again play } (14. h3! { , transposing to the previous line. The centralized knight is indirectly protected and Black's bishop needs to make a choice between allowing an exchange and losing control of the f5-square. I should add that Black can (and should) play } 14... Rfe8! { , trying to exchange the centralized knight. } 15. Qg3!? (15. Nb3 { promises White a moderate initiative after } 15... Nxb3 16. cxb3 Bh5 17. Qg3 Bd6 18. e5 { , but not more than that. }) 15... Qxd4! 16. Bxc7! Rd7! { Black has to
watch out. } (16... Bh5? { loses a queen and the game to } 17. Be5! $18 { . }) 17. hxg4 Bf8! 18. Rad1 Qb4 19. Rxd7 Nxd7 20. Rb1 Nf6 { . Black's activity
compensates for the missing pawn. }) 14... Ne6 { The knight move does not spoil anything, although it might have more accurate to exchange White's active knight for the other bishop, thus doubling White's pawns: } (14... Bxf5 15. exf5 Rfe8 16. g4 { Black's position looks a bit cramped and the knight on c5 currently has no good move, but chess is a very concrete game. Black should equalize through } (16. f6 { is less strong than it looks. Black has at least
two good replies: } 16... Bd6 (16... Bf8!? 17. Qg3 g6 18. Bxc7 Qd2!? $44 { is risky
but sound, as } 19. Red1 { loses outright to } 19... Qxd1+! 20. Rxd1 Rxd1+ 21. Nxd1 Re1# { . }) (16... gxf6 { is not exactly bad, but } 17. Qg3+ Kh8 18. Bxc7!? { looks pleasant for White, as } 18... Rg8? { loses to a queen sacrifice: } 19. Bxd8! Rxg3 20. Bxe7 { Everything hangs and } 20... Rxg2+ (20... Rxc3 21. Bxf6+!) 21. Kxg2 Qg4+ 22. Kf1 Qh3+ 23. Ke2 { allows White's king to escape. }) 17. Qg3 g6) 16... Bd6 17. Qf3 Bxf4 18. Qxf4 Qd6! 19. Qg3 { and now Black can try } (19. Qxd6 cxd6 20. b4?! Nd7 21. Ne4 Ne5! $36) 19... h5!? { with mutual chances. } (19... Qxg3+?! 20. hxg3 { is less accurate, as White's pawns would be better doubled than Black's, controling many important squares on the kingside. The h7-pawn could be vulnerable in some lines involving the g4-g5 advance. })) 15. Nxe7+ Qxe7 16. Bg3 Bh5?! { Given that Black needs to move his f-pawn sooner or later, it made sense to play } (16... f5! { , threatening f5-f4. After } 17. exf5 Bxf5 18. f3 Qb4 { Black gets sufficient counterplay against White's queenside pawns. }) 17. f3! $14 { This move limits the opponent's bishop. White has a slight edge due to his better pawn structure and a more active bishop. He can play for a win without any risk, which is an ideal scenario in a World Championship match. } 17... f6 { Black had to move his f-pawn sooner or later to bring his bishop back into play. That said, such a move also weakens his king a bit. It is not relevant now, but could matter later on. } 18. h3 h6 19. Kh2 { White improves his position little by little. There is no need to hurry, as Black cannot improve his position much, either. } 19... Bf7 20. Rad1 b6 { This is a logical move. The position resembles the Berlin defense, but in our game the pawn formation a5-b6-c5-c7 is more vulnerable, as a loss of the c7-pawn could cost Black another pawn and a game. In Berlin endgames such a danger hardly exists with Black's king on the queenside, and even with Black's king on the kingside White mostly needs to sacrifice a pawn (e5-e6) to win the c7-pawn and perhaps another one. In contrast, in our game the bishop on g3 already eyes c7. After } (20... Rxd1 21. Rxd1 (21. Nxd1!? $14) 21... Rd8 { White could try } 22. Qa7!? { , a move which is no longer possible after 20...b6. That said, Black
holds his own after } { , a move which is no longer possible after 20...b6. That said, Black holds his own after } 22... Rxd1 23. Nxd1 Qb4! { . }) 21. a3 { White could already play } (21. f4 Rxd1 22. Rxd1 $14 { , but protecting the b4-square makes sense. }) 21... a5 22. Ne2 (22. f4!? Rxd1 23. Rxd1 Bh5 $14) 22... Rxd1 (22... c5 $14 { is a good positional move. It weakens the d5-square, but White is unable to exploit it. After } 23. Nf4 { Black should avoid the knight swap with } 23... Nd4! { , as } (23... Nxf4?! 24. Qxf4 { would leave his queenside somewhat exposed. While } 24... Rxd1 25. Rxd1 Rc8 26. e5!? fxe5 27. Qxe5 Qxe5 28. Bxe5 Be8 $14 { is not too bad for Black, he would have weak pawns on both flanks. })) 23. Rxd1 Rd8 24. Rd3 { White maintains the pressure, hoping to exchange the rooks under favorable conditions. After } (24. Rxd8+ Qxd8 25. f4 c5 { Black should not face many problems. In many lines he can sacrifice a pawn, steering into a drawish opposite-coloured bishop endgame: } 26. Qd3 (26. f5 Nd4! 27. Qd3 b5!? 28. Nxd4 Qxd4 29. Qxd4 cxd4 30. Bxc7 a4 31. Bb6 Ba2 32. Bxd4 Bb1 { White's
extra pawn does not matter there. } { White's extra pawn does not matter there. }) 26... Qxd3 27. cxd3 Nd4 28. Nxd4 cxd4 29. f5 c6 30. Bc7 Bb3 31. Bxb6 a4 32. Bxd4 Bc2 33. Kg3 Bxd3 34. Kf3 h5!? $14 { . If White managed to win the c6-pawn, create a passed pawn on f6 and then push b2-b4 to be able to play a3-a4 after Black takes en passant a4xb3, he could generate real winning chances. That said, such a scenario would require too many things going White's way, which is unrealistic on a top level. }) 24... c5 { Black could also play } (24... Rxd3 25. Qxd3 (25. cxd3 c5!) 25... b5!? { with a later a5-a4, moving his queenside pawns outside the reach of White's bishop. Even a loss of the c7-pawn would not be a big problem then, if Black managed to exchange the knights. Knowing the further course of the game, I believe it to be a safer option }) 25. Qd2 c6?! { This move is inaccurate, as it allows an activation of White's pieces and loses a pawn. } (25... Rxd3 26. Qxd3 { is still better for White due to the vulnerability of Black's queenside pawns, but Black should be able to neutralize that advantage with an accurate defense. He can make waiting moves like } 26... Kh7 (26... Be8!? 27. Nc3 c6! { is another option. Then } 28. e5 fxe5 29. Bxe5 b5 30. f4 c4 $14 { is only
slightly worse for Black. }) 27. Nc3 c6 $14 { . It is not much fun, but who says that playing a World Championship match is fun? Top players are used to defending worse positions than this one. }) (25... Kf8 26. Rd5!? (26. Rxd8+ Nxd8) 26... Ke8 27. Qd3 c6 28. Rxd8+ Qxd8 29. Qa6 { is also unpleasant for
Black, but } 29... Kf8! 30. Qb7 Kg8! $14 { brings the king back into safety, while preparing a queen sortie to d2. }) 26. Rxd8+ Nxd8 27. Qf4! { The queen wants to attack the pawns from behind. It seems that Black underestimated this strong maneuver. Ian played this move very quickly. } 27... b5 { Similar was } (27... Bc4!? 28. Nc3 b5 29. Qb8 Kh7 $16 { . }) 28. Qb8 Kh7 { White has achieved a tangible advantage with strong play. That said, Black has many defensive resources. At this moment Black had 13 minutes left for his next 12 moves. } 29. Bd6?! { White spent 14 minutes on this move. He could collect a pawn through } (29. Bc7 Ne6 30. Bxa5 { , but it is not easy to neutralize Black's counterplay after } 30... Qd7 $16 { . That said, White has at least two promising options here: } 31. Nf4! (31. Bc3 Qd1 32. Nf4! Nxf4 33. Qxf4 { , when } 33... Qxc2? { loses a
second pawn to } (33... b4! 34. axb4 cxb4 35. Bxb4 Qxc2 36. Bc3 Qd3 { is a much better defense, as Black is ready to meet } 37. e5 { with } 37... f5! { . His
position is not great but seems to be defensible. }) 34. Qf5+ Kg8 35. Qc8+ Kh7 36. Qxc6 b4 37. Bxb4! Qxb2 (37... cxb4? 38. Qxc2) 38. Qxc5 $18) 31... Nd4!? (31... c4!?) 32. Bb6 Nxc2 33. Bxc5 Ne1! 34. Bd6!? $16 { White limits Black's queen, staying a healthy pawn up. Perhaps Black can still defend with precise play, but White's position is surely promising in practical terms. }) 29... Qd7 30. Ng3 { White could try } (30. Qc7! Qxc7 31. Bxc7 Nb7 32. Kg3 $14 { , when Black's position is defensible but unpleasant. }) 30... Ne6 31. f4?! { White could protect the d4-square with } (31. c3 { , maintaining pressure. A
logical move } 31... c4 { could then be met with } (31... a4!? $14) 32. a4!? bxa4 33. Nf5 $14 { , when more active pieces would give White an edge. True, it is not much after } 33... Nd8! { , as the opposite-squared bishop endgames are mostly drawn. }) 31... h5 { Black could also play } (31... Nd4!? 32. Bxc5 Nxc2 33. Nf5 Bg6 34. Ne7 Qe8 (34... Be8)) 32. c3 { 1/2-1/2 The game is a draw. } 32... c4 { Black could play } (32... h4! 33. Nf5 Bg6 $10 { , as } 34. Ne7 Be8 { does not promise White more than equality. His pieces are active but also hang in many lines. Right now Black would like to play 35...Nxf4! }) 33. h4 { White prevents the h5-h4 ideas. } 33... Qd8 34. Qb7 { This move requires a good reaction from Black. On the other hand, the correct reaction resolves the problems. White could try } (34. Qxd8!? Nxd8 { and now perhaps } 35. f5!? (35. a4!?) 35... Nb7 36. Be7 a4 37. Ne2 $14 { with 38.Kg3 to follow. Black should be able to defend, but I can imagine Magnus Carlsen happily pushing for many decades of moves in such a position. }) 34... Be8! { The only move, but a sufficient one. After } (34... Qxd6? 35. Qxf7 { White wins easily: } 35... Qxf4 (35... Nxf4 36. Nf5 $18) 36. Qxe6 Qxh4+ 37. Qh3 $18) 35. Nf5 Qd7 36. Qb8 Qd8 37. Qxd8 Nxd8 38. Nd4 (38. a4!?) 38... Nb7 39. e5 { After } (39. Be7 Kg8 { White's bishop lacks retreat squares. }) 39... Kg8 40. Kg3 Bd7 { The time control is over. Ding Liren has consolidated his position. } 41. Bc7 (41. e6?! Bxe6!) 41... Nc5 { Black temporarily gives a pawn. He must have been happy to activate the knight which had largely been performing defensive tasks until now. } 42. Bxa5 Kf7 43. Bb4 Nd3 44. e6+ Bxe6 45. Nxc6 Bd7 46. Nd4 Nxb2 $10 { The position is dead equal now, as a knight swap almost invariably leads to a simple draw. } 47. Kf3 Nd3 48. g3 Nc1 { The knight heads for b3, trying to exchange White's centralized knight. } 49. Ke3 Nb3 { An interesting strategic game. It contained no fireworks, but we have seen a high-level fighting game, not a boring theoretical draw. Ian Nepomniachtchi achieved to receive a fresh position and build up an advantage with strong play, but Black's position remained hard to crack even after some inaccuracies. Starting from move 27, Ding Liren demonstrated a high-level defense and made a draw. With the current level of preparation and play, it is very hard to win a classical game against a top player who acts solidly. That said, I am sure that we will see such games soon. (I swear that I had written this before the start of game 2!) } 1/2-1/2

[Event "FIDE World Championship 2023"]
[Site "Astana, Kazakhstan"]
[Date "2023.04.07"]
[Round "2.1"]
[White "Liren, Ding"]
[Black "Nepomniachtchi, Ian"]
[Result "0-1"]
[WhiteElo "2788"]
[BlackElo "2795"]
[Annotator "Navara,David"]
[UTCDate "2023.04.11"]
[UTCTime "07:41:19"]
[Variant "Standard"]
[ECO "E10"]
[Opening "Indian Defense: Anti-Nimzo-Indian"]

1. d4 Nf6 2. c4 e6 3. Nf3 d5 4. h3!? { A very rare continuation as early as on move 4! It leads to fresh positions, or rather to fresh variations on well-known topics. } 4... dxc4 { Ian Nepomniachtchi spent 9 minutes on this move.
Black had a wide choice, but needed to show some cards, as some of the available plans cannot be combined. (Say, both c7-c6 and c7-c5 are reasonable options here, but playing c7-c6-c5 is rarely good here.) Ian Nepomniachtchi decided to steer play into the Queen's Gambit Accepted, where h2-h3 is not the most useful move. } (4... c5!? { looks very reasonable, as the engine's top line } 5. cxd5 (5. e3 { is always an option, but hardly dangerous for Black. }) 5... exd5 6. g3 { does not look impressive with the inclusion of the h2-h3 advance, which somewhat weakens White's kingside. }) (4... c6 5. e3 { leads to a Slav position where h2-h3 is a reasonable move, though not the most ambitious one. } (5. cxd5 exd5 6. Bf4 Bf5)) (4... a6!? { is a witty reply to White's previous move. White can react with } 5. cxd5 exd5 6. Bf4 { , hoping for a
slight edge. }) (4... Be7 5. cxd5 exd5 6. Bf4 { leads to a line where the h2-h3
advance is useful, although Black should be doing well after } 6... c6 7. Nc3 Bf5 8. g4 Be4!? { . }) (4... Nbd7 5. cxd5 exd5 6. Bf4 Bb4+!? { is another case of a
dynamic equilibrium. }) 5. e3 c5 6. Bxc4 a6 7. O-O { We have transposed to a Queen's Gambit Accepted with a rather unusual h2-h3 move, which is not bad at all. } 7... Nc6 { This move cost Black another 9 minutes. } (7... b5 8. Be2 Nbd7 { was a relevant alternative. White might have a slight initiative after } 9. a4!? b4 10. Nbd2 Bb7 11. b3 { , but the position does not look too scary for Black. }) 8. Nc3 (8. dxc5 Qxd1 9. Rxd1 Bxc5 { leads to a symmetric position
where White cannot hope for much. That said, } 10. Nfd2!? O-O 11. Nb3 Be7 12. Nc3 { still promises him mild initiative. }) 8... b5 9. Bd3 Bb7 { Black managed to develop his pieces. Such positions are considered good for him, but Ding Liren had a concrete idea in mind. } (9... cxd4!? 10. exd4 Nb4!? { was a more
circumspect option, as Black should be doing well after } (10... Be7 11. a4 bxa4 { is another relevant option, but } 12. Ne5 { requires some precision from Black: } 12... Bb7! 13. Rxa4 (13. Qxa4 Qxd4! 14. Nxc6 Qxa4 15. Rxa4 Bxc6 16. Rxa6 Rxa6 17. Bxa6) 13... O-O 14. Bxa6!? Rxa6!? 15. Rxa6 Nxe5 16. Ra7 Qb6 (16... Nf3+!? 17. gxf3 Qb8 $14 { is a braver option, but it seems that an exchange coupled with an extra pawn are worth slightly less than Black's activity and structural advantage. }) 17. Rxb7 Qxb7 18. dxe5 Nd7 $44 { White is a pawn up,
but Black's pieces are more active. }) 11. Be2 Be7 12. a4 bxa4 13. Nxa4 O-O { . }) 10. a4! { White immediately attacks Black's queenside before the opponent could complete development. } 10... b4 (10... c4?! 11. axb5! axb5 12. Rxa8 Bxa8 (12... Qxa8?? 13. Nxb5 cxd3 14. Nc7+) 13. Bb1 b4 (13... Qa5 14. e4 $14 { gives White a central control, with both advances d4-d5 and e4-e5 being
dangerous for Black. }) 14. Ne4 { Black has to tread carefully to avoid problems with his weak c4-pawn or undeveloped kingside }) (10... cxd4 { gives White an extra option } 11. axb5!? (11. exd4 { might transpose to 10...b4 followed by 11.
..cxd4. }) 11... dxc3 12. bxc6 Bxc6 { . Computer slightly prefers White's position, although it is hard to achieve anything tangible here. } 13. Nd4!? (13. Ne5 Bb5 14. bxc3 Bxd3 15. Nxd3 Be7) 13... Bd7 14. bxc3 (14. Qc2!? cxb2 15. Bxb2 Bd6 16. Rfd1 O-O 17. Nb3 Be7! 18. Nc5 Kh8!? { should be safe for Black,
as } 19. Nxd7 Qxd7 20. Bxh7 { allows } 20... Qc8! { with a queen exchange. }) 14... e5 15. Ne2 $14) 11. Ne4 Na5!? { A good move, and possibly also an unexpected one. } (11... cxd4 12. exd4 Be7 13. Re1 O-O 14. Be3 $36 { looks preferable for White, as Black has weak squares on the c-file }) (11... Qd7 { was another relevant option. } 12. a5 (12. Nxc5 Bxc5 13. dxc5 O-O-O! 14. Be2 Qxd1 15. Bxd1 a5 { Black is going to regain a pawn without allowing the activation of
White's bishop pair. } (15... Ne4 { might be less accurate, as the following
sharp line promises White more than enough compensation for a pawn: } 16. a5! Nxc5 17. Ng5! Rd7 18. e4 h6 19. Be3 Nd3 20. Nf3 Nxb2 21. Be2 Nd3 22. Nd2! Kb8 23. Nc4 $14)) 12... cxd4 13. exd4 Be7 14. Bf4!? (14. Be3) 14... O-O 15. Nc5 Bxc5 16. dxc5 Rfd8 17. Be2 $14) 12. Nxf6+ { White spent 33 minutes on this move. He had played quickly until now. My first independent move often turns out to be inaccurate. It occasionally happens even to stronger players. } (12. Nxc5! Bxc5 13. dxc5 { was more promising. Black can regain the pawn in many ways, but White has a bishop pair in an open position, which often ensures him an edge. } 13... Be4! { is the critical option, which looks very good for Black at first sight and not bad under a closer inspection, either: } (13... Nd7 14. c6!? { This move brings some a shade of disharmony in Black's setup. Black would prefer taking on c5 with his other two minor pieces remaining in their positions. } 14... Bxc6 (14... Nxc6 15. Qe2 Nce5 (15... Nc5 { is a relevant option even here, but the other knight would be slightly better placed on a5 than on c6. }) 16. Nxe5 Nxe5 17. Bc2 $14) 15. Nd4 Nc5 16. Bc2 $14) 14. Bxe4 Qxd1 15. Rxd1 Nxe4 16. Bd2!? (16. c6!? Nb3 (16... Nxc6?! 17. Bd2 Rd8 18. Rac1 $14 { causes Black problems with his queenside weaknesses. }) 17. Rb1 Rd8! 18. Nd4! Ke7 19. c7!? Rd6 20. f3 Nec5 21. Bd2 a5 22. Nxb3 Nxb3 23. Be1 { . Now Black should be able to regain the pawn after } 23... f6 24. Bg3 Rc6! (24... e5? { is surprisingly bad in view of } 25. f4! { , when White's bishop enters play with a decisive effect, e.g. } 25... Ke6 26. fxe5 fxe5 27. Rxd6+ Kxd6 28. Rd1+! Kxc7 29. Bxe5+ $18) 25. Rd3 Nc1! { with equality. All these lines look risky for White, whose queenside is underdeveloped. }) 16... Nxd2! (16... Nb3?! 17. Bxb4 Rb8 18. Ba3 Nxa1 19. Rxa1 { slightly favors White. If Black castles, his king might be too far from the queenside pawns. If he directs his king to the queenside instead, his monarch might become a target. }) 17. Nxd2 O-O-O! 18. Ne4 Nb3! 19. Nd6+ Rxd6 20. cxd6 Nxa1 21. Rxa1 Kd7 22. Rc1 Kxd6 { is a sharp line leading to a drawish endgame. }) 12... gxf6 13. e4?! (13. dxc5!? Qd7!? (13... Bxc5 { and }) (13... Rg8 { are relevant options as well. }) 14. c6! Qxc6 (14... Bxc6 15. Nd4 Bb7 16. Qe2 Rg8 17. f3 $13) 15. e4) 13... c4! 14. Bc2 Qc7 $15 { Black has achieved a favorable pawn structure. His queenside majority will protect the king on b8 and might become a big asset in the endgame. He can attack along the g-file. White currently controls the center, but Black can easily attack it with his pieces and with the f6-f5 push. Ian Nepomniachtchi is an experienced Gruenfeld player, so he knows well how to undermine a pawn center! } 15. Bd2 Rg8 (15... O-O-O { was also an option, and possibly a slightly better one. The game has demonstrated that Black achieves an advantage if he manages to bring his king into safety without losing anything. } 16. Qe1 Qb6 17. Bf4 Bd6 18. Qe3 Kb8 $15) 16. Rc1 { This is a very
logical move, but even more concrete action was needed. } (16. Qe1! f5! 17. Qe2! (17. d5 O-O-O! 18. Bxb4 Bxb4 19. Qxb4 fxe4 20. Bxe4 Qf4! 21. Qc5+ Kb8 22. Qe3! Qxe3 23. fxe3 f5! 24. Bc2 Bxd5 25. Rae1 Nc6 26. e4 fxe4 27. Bxe4 $15) 17... O-O-O 18. Rad1 { leads to a position with mutual chances.
Black's position might be easier to play, but both sides need to play
precisely. }) 16... O-O-O 17. Bd3?! { This is logical but wrong. White spent 15 minutes on this move and then played the next one very quickly. } (17. Qe1! f5! 18. Kh1 Nc6! { also seems to favor Black, but } 19. Qe2! Nxd4 20. Nxd4 Rxd4 21. f3 fxe4 22. Bxe4 Bxe4 23. fxe4 Bd6 24. e5! Bxe5 25. Bxb4 $132 { gives White reasonable counterplay. }) 17... Kb8 { Before pushing f6-f5, Black brings his king into (relative) safety. } 18. Re1?! { This does not work, but
nor did a concrete action: } (18. Bxc4!? { I guess that the Chinese grandmaster planned this from afar, but spotted some important nuance only after move 16. } 18... Nxc4 19. Qe2 { looks good before one finds } (19. b3 Nb2! (19... Nxd2? 20. Rxc7 Nxf3+ 21. Qxf3 Kxc7 22. Qxf6 { allows White to generate counterplay against Black's weak king: } 22... Rd7 23. d5! exd5 24. Qf4+! Kb6 (24... Bd6? { loses material to } 25. e5! Bf8 26. e6+ Bd6 27. Qc1+! $18) 25. Qb8! dxe4 26. a5+! Kxa5 27. Qe8! $44 { with a possible move repetition after } 27... Bc6 28. Qb8 Bb7 29. Qe8 { . }) (19... Bxe4! { This is simple and strong. } 20. Rxc4 Qb7 21. Bf4+ Bd6 22. Bxd6+ Rxd6 23. Kh2 Rc6! 24. Rxc6 Qxc6 $17 { The material is
equal, but Black has a huge positional advantage. His pieces are more active. In the middlegame White has problems with his king and the g2-pawn, whereas in endgames the b3-pawn could easily become vulnerable. }) 20. Rxc7 (20. Qe2?! Qd6 21. Rc2 f5! { allows White to win the knight back, but in the meantime Black opens the long diagonal with a decisive attack. }) 20... Nxd1 21. Rxb7+ Kxb7 22. Rxd1 Be7 $17 { gives Black a technically winning position. }) 19... Rc8! (19... Nxd2? 20. Rxc7 Nxf3+ 21. Qxf3 Kxc7 22. Qxf6 { is a much worse version, as the
king is exposed on c7. }) 20. b3 Nxd2 21. Rxc7 Nxf3+ 22. Qxf3 Rxc7 { , when
Black has both material and positional advantage. }) (18. Kh1 f5! 19. Qe1 Qb6 20. Bc2 $17 { also favors Black, but looks less one-sided. }) (18. Qe1 f5! 19. Kh1 Qb6 $17 { transposes to the previous line. }) 18... f5! 19. Bc2 Nc6! { The e4-pawn is currently well protected, but it turns out that Black can also attack d4! } (19... f6!?) 20. Bg5 (20. Bb1 fxe4 21. Bxe4 f5! 22. Bxc6 Qxc6 { gives Black a strong pressure along the long diagonal, coupled with a positional advantage due to a bishop pair and a favorable queenside structure. }) 20... Rxg5! { Ian Nepomniachtchi is in his element. He sacrifices an
exchange to further activate his pieces. } 21. Nxg5 Nxd4 22. Qh5 f6 (22... Bc5!?) 23. Nf3? { White faced a very difficult choice, but this move simplifies
Black's task. Given that all the options were bad, it made sense to complicate the position. True, the resulting positions are not only highly advantageous for Black, but usually also easier to play for him in terms of avoiding mistakes. } (23. Nxh7! Bc5 { was also bad for White, but } 24. Bd1! { forces
Black to make some accurate steps on each of the ways to Rome. } (24. Nxf6 Qf4! { is not over yet, but it is winning for Black. } 25. Red1 Ka7!) 24... Bd6!? (24... fxe4 { This might be winning, but Black needs to find many precise moves on the way to the whole point. } 25. Rxc4 Nf5! 26. Rxc5 (26. Qe2 Bxf2+!) 26... Qxc5 27. Nxf6 Rd2 28. Ng4 Qd6! { A very important move, which is easy to miss. } 29. Bb3 (29. Be2 Rxe2! 30. Qe8+ Bc8! { The bishop protects
the e6-pawn. } 31. Rxe2 Qd1+ 32. Kh2 Qxe2 $19) 29... Nd4! 30. Bc4 Nc2! 31. Rf1 e3! $19) 25. Nxf6 { This is far from forced, but the alternatives are
similarly bad. } 25... Bh2+ 26. Kh1 Bf4 27. Rb1 (27. Ra1 c3!? 28. bxc3 Qxc3! { leads to a rare fork. In fact, even White's knight is hanging in many lines, e.
g. } 29. Bf3 Bd2 30. Red1 Nxf3 31. Qxf3 Qxf6 { . }) 27... Bd2 28. Rf1 Qg7 29. Qh4 Bg5 30. Nh5 Bxh4 31. Nxg7 Bxe4 32. Rc1 Bd3 $19) (23. exf5?! fxg5 24. fxe6 { is bad for many reasons, as Black has a slight material advantage and dominates the board. }) (23. Nf7 Rd7 24. exf5 (24. Nh6 { leaves White's pieces
in complete disarray. Black has numerous winning continuations, } 24... Nxc2 25. Rxc2 b3 26. Rcc1 Bb4 27. Red1 Bd2! 28. Ra1 Bxe4 $19 { being one of them. }) 24... Nxc2 25. Rxc2 e5!? 26. Nxe5 fxe5 27. Qe8+ Rd8 28. Qxe5?! Qxe5 29. Rxe5 Rd1+ 30. Kh2 Bd6 31. f4 { and now many continuations win, but it might be better to
postpone taking on e5, first protecting the c4-pawn with a move like } 31... Rd4!? $19 { . }) 23... Nxc2 24. Rxc2 Bxe4 { This is simple and strong. } (24... b3!? { was even more resolute, e.g. } 25. Rd2 (25. Rcc1 fxe4 $19) 25... Bb4! 26. Rxd8+ Qxd8 27. Rf1 fxe4 $19) 25. Rd2 (25. Rcc1!? Bc5! 26. Qh4 Rg8 27. Kh1 Qg7 28. Rg1 { would have avoided an immediate catastrophe at the cost of a long
suffering with very little hope. Black has many good moves, including } 28... c3 29. bxc3 b3! 30. Qf4+ Ka8 31. Qd2 Qb7! { with a decisive advantage. The difference in piece activity is striking. }) 25... Bd6 $19 { The game is over. The bishop pair controls the board. Moreover, Black's c-pawn can hardly be stopped. } 26. Kh1 { Black was threatening to exchange on f3, give a check on h2 and then take a rook. } (26. Nd4 c3 27. bxc3 bxc3 { was equally bad for White,
who has no time to capture on e6: } 28. Rdd1 (28. Nxe6 cxd2! $19) 28... c2 29. Nxe6 cxd1=Q 30. Rxd1 Bh2+! $19) 26... c3 27. bxc3 bxc3 28. Rd4 c2 29. Qh6 e5 { White resigned, as he loses a lot of material without getting anything in return. Ian Nepomniachtchi was surely surprised by Ding Liren's move 4, but with healthy moves he achieved a good position, overtook initiative, gained central control with a strong exchange sacrifice and then went on to convert his advantage. A convincing victory with Black! That said, the match has just begun and there are many games left. Hopefully the Chinese grandmaster recovers from a loss and we will be able to see a dramatic match of two elite players! } 0-1

[Event "FIDE World Championship 2023"]
[Site "Astana, Kazakhstan"]
[Date "2023.04.12"]
[Round "3.1"]
[White "Nepomniachtchi, Ian"]
[Black "Ding, Liren"]
[Result "1/2-1/2"]
[WhiteElo "2795"]
[BlackElo "2788"]
[Annotator "Navara,David"]
[UTCDate "2023.04.14"]
[UTCTime "07:55:51"]
[Variant "Standard"]
[ECO "D35"]
[Opening "Queen's Gambit Declined: Exchange Variation, Positional Variation"]

1. d4 { A slight surprise for many people, as Ian Nepomniachtchi mostly plays 1.
e4. That said, players prepare for such matches for many months, so this in
itself cannot surprise the opponent. } 1... Nf6 2. c4 e6 3. Nc3 { The Russian
grandmaster played this move rarely, mostly in speed games played until 2021.
True, he played the Nimzo in a game against Andrei Esipenko in February 2023,
in his last classical tournament before the match. } (3. Nf3 { was a bit more
common move in Ian's games, although he also used it mostly in speed games. }) 3... d5 (3... Bb4 { is the most common move on a top level. Ding Liren also
mostly chooses it, but Ian Nepomniachtchi has surely prepared something there. }) 4. cxd5 exd5 5. Bg5 c6 6. e3 { The Carlsbad structure is strategically very
complex, as both sides have a multitude of plans. The resulting positions
mostly require good understanding rather then a precise calculation or
imagination. White has slightly easier play, but Black has a solid position.
Carlsbad ("Karlsbad" in German, "Karlovy Vary" in Czech, "Карловы
Вары" in Russian) is a lovely Czech spa town near the border with Germany,
very popular among Russians. Even the legendary chess trainer and writer Mark
Dvoretsky was travelling there often. } 6... h6 { This push had been considered
inaccurate until recently, but has become very popular during the last 10
years. Black can no longer transfer his knight to g6, but it has turned out
that a bishop exchange leads to a satisfactory position. } 7. Bh4 Be7 8. Bd3 O-O 9. Qc2 Re8 10. Nge2 Nbd7 11. O-O (11. O-O-O? { is too optimistic, as Black's
attack comes first. Here is a sample variation: } 11... b5! 12. Rdg1 (12. g4 Nxg4 { does not give White sufficient compensation. }) 12... Nb6 13. Bxf6 Bxf6 14. h4 b4 15. Nd1 h5! 16. Nf4 c5 17. dxc5 Bd7 18. Nxh5 Rc8 19. Nxf6+ Qxf6 $17 { with a strong attack. }) (11. f3!? { avoids the
bishop exchange, but Black can again continue with } 11... b5!? 12. O-O Bb7 13. Rac1 a6! { , preparing c6-c5. Now } 14. e4?! { invites strong counterplay: } (14. Qd2 c5 15. dxc5 Nxc5 { leaves both pawns e3 and d5 equally weak. }) 14... b4! { If you dislike long variations, feel free to skip them. They just show
that the untimely e3-e4 push allows Black to take over initiative. } (14... dxe4 15. Nxe4 (15. fxe4? c5! 16. Bxf6 Bxf6 17. e5 Bg5 $17 { is already bad for
White. }) 15... Nxe4 16. Bxe7 Qxe7 17. Bxe4 (17. fxe4 Rac8 $36 { followed by
c6-c5 is pleasant for Black. }) 17... Rad8) 15. e5 (15. Na4 dxe4! 16. fxe4 c5! (16... Nxe4!? 17. Bxe4 Bxh4 { is a relevant motif which I missed in
several games here on Lichess. Right now it only leads to an unclear position
after } 18. Bh7+! Kh8 (18... Kf8?! 19. Qc4) 19. Rxf7 { . }) 17. Nxc5 (17. e5? Nd5 $17 { creates problems with a fork on e3. }) 17... Nxc5 18. dxc5 Rc8 19. Bc4 Kh8! (19... Bxc5+ 20. Kh1 Be3 21. Bxf6 gxf6 { is also an option, but a
dangerous-looking one. }) 20. Bxf7 Bxc5+ 21. Kh1 Bxe4 22. Bxf6 gxf6 23. Qd1! Qxd1 24. Rfxd1 Bxg2+! 25. Kxg2 Rxe2+ 26. Kh1 Rxb2 27. Bb3! { White
threatens 28.Rd5. } 27... Re2 28. Bc4 $15 { White regains one pawn and should hold
rather easily. That said, it is not an inspiring line from White's point of
view, anyway. }) 15... bxc3 16. exf6 Bxf6 17. Bxf6 Qxf6 18. bxc3 (18. Qxc3 Nf8! $36 { Black has initiative in spite of his passive bishop. White has a weak
square on e3 and a weak central pawn. Black cannot invade there immediately
because of 20. Bh7+, but he can double the rooks or just play 19...a5,
improving a queenside structure. Moving the knight to g3 would weaken d4 and
f4. }) 18... a5 { Black will exchange the bishops on a6 with good play. His
knight could then be transferred to c4. }) 11... a5 { Black makes a useful move.
The weakening of the b6-square is not a big problem. Black might play b7-b5
later. } (11... Nh5 12. Bxe7 Qxe7 { leads to a typical position which might be
slightly better for White according to Stockfish, but it is very hard to prove
that in a game. }) (11... Ne4 { is a more straightforward approach. After } 12. Bxe7 Qxe7 13. Bxe4 dxe4 14. Rfd1 Nf6 15. d5 { or } (15. h3!? { White seems to be
slightly better, though. })) 12. a3 (12. f3 b5 13. Bf2 { and }) (12. Rfe1!? Nh5 13. Bxe7 Qxe7 14. h3 $36 { are two of many alternatives to the game
continuation. }) 12... Nh5 (12... b5 { is another option. White cannot exploit a
weakening of the c5-square here, as his knights cannot get there easily. }) 13. Bxe7 Qxe7 14. Rae1 { It often makes sense to play for e3-e4 once Black has
played a7-a5, as the typical push c6-c5 has become problematic because of the
weakening of the b5-square. } 14... Nf8 15. Nc1 (15. Na4 { was a reasonable
alternative, but Black remains solid after } 15... Nf6 { . It is hard improve White's
position. }) 15... Nf6 16. f3 { If being given enough time, White would like to
push e3-e4. That said, Black can prevent that by attacking the d4-pawn. } 16... Ne6 { All this has already been played in an online rapid game Giri - Ding in May
2022. } 17. N1e2 { The knight returns now when Black's queen no longer attacks
e3. That said, this plan is not very dangerous for Black. } (17. Qd2 Rb8 { should also be equal. Black can choose between b7-b6 (often followed by c6-c5)
and b7-b5-b4 depending on White's move, for instance } 18. Nb3 (18. N1e2 b5! $132) 18... Qd8! { Black prevents e3-e4. } (18... b6?! 19. e4! { allows
White to execute a strategically desirable advance at the right moment. }) 19. Kh1 Kh8!? { The king avoids a check on h7, thus complicating the e3-e4 advance.
(If you ask for a more detailed explanation, please look at the 19...b6 20.e4
line.) } (19... b6 { allows } 20. e4 { , when } 20... dxe4?! (20... Nh5!) 21. fxe4 Nxd4? { fails tactically to } 22. Nxd4 Qxd4 23. e5! Rd8 (23... Rxe5? { immediately loses to } 24. Bh7+! Kxh7 25. Qxd4 $18) 24. exf6 Qxd3 25. Qf4! { White attacks both rooks at the same time, as 26.Rd1 is also a threat. } 25... Bb7 26. fxg7 Qg6 27. Ne4 c5 28. Nf6+ Kxg7 29. Rf2 { with a crushing attack. We have
gone too far, but lines like this are typical for the Carlsbad structure. A
recent blitz game Navara - Alonso Rosell from the European Blitz Championship
2022 saw such a scenario. We played a different opening, yet the similarities
are striking. }) 20. Qf2 b6 21. e4 Nf4! 22. Bb1 Ba6 23. Rg1 { Here Black needs
to do something not to get steamrolled after e4-e5. There are several ways to
counter this scenario, including } 23... dxe4 (23... Bd3!?) (23... c5!?) 24. fxe4 Nd3 25. Bxd3 Bxd3 26. e5 Ng4!? 27. Qg3 Bc4 { with mutual chances. It is
fascinating how quickly can a positional struggle transform into a tactical
skirmish! Given how many razor-sharp opening lines have been analyzed to a
draw, playing "slower" openings like Carlsbad makes a lot of sense even for
sharper players. }) (17. Qf2 b5! { looks good for Black, who is ready to
push b5-b4 and then perhaps develop his bishop to a6. } (17... c5 18. Bb5! Rd8 19. dxc5 Qxc5 20. Nb3 Qb6 21. Rd1 Bd7 22. Bxd7 Rxd7 23. Nd4 Re8 { happened in
the aforementioned Giri - Ding game. White seems to have a minute edge after } 24. Rfe1 { or } (24. Na4))) 17... c5 (17... b5! { was a good option, here. It
would be bad if White could settle his knight on c5, but it is unrealistic.
Concrete attempts to attack c6 or c5 fail: } 18. Bf5?! (18. Nd1 Ba6!? 19. Qxc6?! Rec8 20. Qb6 Bb7! { Here White would have to exchange the queen for a rook,
a bishop and two pawns. It is more than enough material, but White's pieces
would not be placed too harmoniously and the f2-f3 advance could cause some
problems to White's king, as the pawns do not go backwards. } 21. Bxb5 (21. Qxb5 Ba6! 22. Qxa6 Rxa6 23. Bxa6 Rc2 $36) 21... Rcb8! 22. Bd3 Ba6 23. Qxa6 Rxa6 24. Bxa6 h5 $36) 18... b4 19. axb4 axb4 20. Nd1 Ba6 { and now } 21. Qxc6? { loses to } 21... Bxe2 22. Rxe2 Nxd4! 23. exd4 Qxe2 $19) 18. Bb5?! { White only had
a slight initiative, but this move allows Black to equalize. } (18. Nf4! cxd4 (18... Nxf4 19. exf4 Qxe1? 20. Rxe1 Rxe1+ 21. Kf2 Re8 22. dxc5 $16 { is very
bad for Black, who lacks counterplay. White will just transfer the knight to
d4 and then start pushing pawns on the kingside or even on the queenside. }) 19. exd4 Qd8 20. Nxe6 Bxe6 21. Qf2 $36 { It is not much, but a risk-free edge is
welcome in such matches. }) 18... Rd8 19. dxc5 (19. Qd2!? b6 20. Ng3 { would
have maintained the tension, but Black is doing well. Computer suggests
strange moves like } 20... Qf8 { (bringing the queen to a safer place) and } (20... Ra7 { , preparing a rook transfer to e7 or d7 (after Bc8-d7). })) 19... Qxc5 20. Qd2 Bd7 (20... d4 21. exd4 Nxd4 22. Nxd4 Qxd4+ 23. Qxd4 Rxd4 { should also lead to
equality, but White would still have a very slight initiative. In the game
Ding Liren soon overtook the initiative, therefore his choice looks better. }) 21. Bxd7 (21. Nd4 Rac8 22. Rc1 (22. Nxe6 fxe6) 22... Nxd4 23. exd4 Qd6) 21... Nxd7 22. Nd4 Nb6 { Black has reached a comfortable position. } 23. Rd1 Nc4 24. Qf2 Rac8 { Black maintains the pressure. } (24... Nxa3 { is also equal if
White finds } 25. e4! { . There is no doubt that both players saw that. } 25... Nxd4 (25... dxe4?? { loses a piece to } 26. Nxe6 (26. Nxe4 $18) 26... Qxf2+ 27. Kxf2 fxe6 28. bxa3 $18) 26. Rxd4 Nb5 27. Nxb5 (27. Rxd5 Qxf2+ 28. Kxf2 Nxc3 29. Rxd8+ Rxd8 30. bxc3 b5 31. Rb1 Rb8 32. Ke3) 27... Qxb5 28. Rfd1 { White regains a pawn with equality. }) 25. Na4 Qe7 26. Rfe1 Qf6 (26... Nxd4!? 27. Rxd4 (27. exd4 Qf6 $15) 27... b5 28. Nc3 Qc5 { looks equal, but with the
pawns a3 and b2 hanging, White needs to be a bit careful: } 29. b4!? (29. Red1 Nxb2 30. Qxb2 Qxc3 31. Qxc3 Rxc3 32. Rxd5 Rxd5 33. Rxd5 Rxe3 34. Rxb5 Rxa3 35. h4 $15 { leads to a drawish endgame. Some versions of such an endgame can be
very tricky, but here Black's king is passive and the kingside pawn structure
is better for the defender than usually, so it would be a simple draw. }) 29... Qc6 30. Red1!? Nxe3 31. Qxe3 Re8! (31... Qxc3 32. Qxc3 Rxc3 33. Rxd5 Rxd5 34. Rxd5 Rxa3 35. Rxb5 { leads to mass exchanges and a dead draw. }) 32. Ne4!? dxe4 33. bxa5 exf3 34. Qxf3 Qxf3 35. gxf3 Ra8 36. a4 Rxa5 37. axb5 Rxb5 38. Rd8 { is another very drawish endgame where an extra pawn cannot be converted. }) (26... Qe8!? 27. Nc3 Nxa3 { was more ambitious but also somewhat risky. White
equalizes after } 28. Nf5 Rc5 29. Qg3 (29. Nd4!? b6 30. e4 { might also lead to
equality. }) 29... Kf8 (29... Nc4 { looks very dangerous, as Black's king will
be exposed once the h6-pawn gets off the board. That said, White needs to be
more accurate in the ensuing complications: } 30. Nxh6+ Kf8 31. Nf5 g6 32. e4! d4 (32... gxf5? 33. exf5) 33. Qh3! gxf5 34. exf5 Rxf5! 35. Qxf5 dxc3 36. Qh7 { White also has other moves here. } 36... Rxd1 37. Rxd1 c2! 38. Qh8+! (38. Qxc2? Ne3! $19) 38... Ke7 39. Qh4+ f6 40. Re1! Ne5 41. Qh7+ Kf8 42. Qxc2 $13 { with a dynamic equilibrium. }) 30. e4! d4 31. Rxd4! Rxd4 32. Nxd4 Nc4 (32... Nxd4 33. Qd6+ Qe7 34. Qb8+! Qe8 35. Qd6+ { leads to a move repetition. }) 33. Nxe6+ Qxe6 34. Qf2) 27. Nb5 Nc7 28. Nd4 Ne6 (28... b5 29. Nc3 { is also
equal, e.g. } 29... b4 30. axb4 axb4 31. Na2! (31. Na4 Ne6 { is pleasant for
Black. }) 31... b3 { and now any knight retreat maintains equality, while } 32. Nxb3?! (32. Nc1) (32. Nc3) 32... Nxb2 $15 { followed by 33...Nc4 gives
Black a slight pull due to disorganized White's pieces, but not more. }) 29. Nb5 Nc7 30. Nd4 Ne6 { Ding Liren has recovered from the previous loss and played a
good game. He equalized easily and even had slight initiative towards the end.
On the other hand, White was never in real danger. The game was relatively
short, with most of the action remaining behind the curtains. Ian
Nepomniachtchi maintains the lead, while Ding Liren needs only one win to
level the score. } 1/2-1/2
  """.trim
