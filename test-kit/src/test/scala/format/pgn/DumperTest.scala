package chess
package format.pgn

import scala.language.implicitConversions
import chess.format.{ EpdFen, Fen }
import Square.*

import chess.variant.ThreeCheck

class DumperTest extends ChessTest:

  given Conversion[String, SanStr] = SanStr(_)

  test("Check with pawn not be checkmate if pawn can be taken en passant"):
    val game = Fen.readWithMoveNumber(EpdFen("8/3b4/6R1/1P2kp2/6pp/2N1P3/4KPPP/8 w - -")).get match
      case s: Situation.AndFullMoveNumber => Game(s.situation, ply = s.ply)
    val move = game(Square.F2, Square.F4).get._2
    assertEquals(Dumper(move), "f4+")

  val gioachineGreco = makeGame.playMoves(
    D2 -> D4,
    D7 -> D5,
    C2 -> C4,
    D5 -> C4,
    E2 -> E3,
    B7 -> B5,
    A2 -> A4,
    C7 -> C6,
    A4 -> B5,
    C6 -> B5,
    D1 -> F3
  )

  val peruvianImmortal = makeGame.playMoves(
    E2 -> E4,
    D7 -> D5,
    E4 -> D5,
    D8 -> D5,
    B1 -> C3,
    D5 -> A5,
    D2 -> D4,
    C7 -> C6,
    G1 -> F3,
    C8 -> G4,
    C1 -> F4,
    E7 -> E6,
    H2 -> H3,
    G4 -> F3,
    D1 -> F3,
    F8 -> B4,
    F1 -> E2,
    B8 -> D7,
    A2 -> A3,
    E8 -> C8,
    A3 -> B4,
    A5 -> A1,
    E1 -> D2,
    A1 -> H1,
    F3 -> C6,
    B7 -> C6,
    E2 -> A6
  )

  val threeCheck = Game(Board.init(ThreeCheck)).playMoves(
    E2 -> E4,
    C7 -> C5,
    F1 -> C4,
    B8 -> C6,
    C4 -> F7,
    E8 -> F7,
    D1 -> H5,
    G7 -> G6,
    H5 -> G6
  )

  test("standard game"):
    gioachineGreco
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, SanStr.from("d4 d5 c4 dxc4 e3 b5 a4 c6 axb5 cxb5 Qf3".split(' ').toVector))
    peruvianImmortal
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(
          ms,
          SanStr.from(
            "e4 d5 exd5 Qxd5 Nc3 Qa5 d4 c6 Nf3 Bg4 Bf4 e6 h3 Bxf3 Qxf3 Bb4 Be2 Nd7 a3 O-O-O axb4 Qxa1+ Kd2 Qxh1 Qxc6+ bxc6 Ba6#"
              .split(' ')
              .toVector
          )
        )

  test("three check variant"):
    threeCheck
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, SanStr.from("e4 c5 Bc4 Nc6 Bxf7+ Kxf7 Qh5+ g6 Qxg6#".split(' ').toVector))

  test("without check"):
    val game = Game("""

P    k




PP   PPP
KNBQ BNR
""")
    game
      .playMoves(A7 -> A8)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("a8=Q")))
  test("with check"):
    val game = Game("""
  k
P




PP   PPP
KNBQ BNR
""")
    game
      .playMoves(A7 -> A8)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("a8=Q+")))
  test("with checkmate"):
    val game = Game("""
    k
P  ppp




PP   PPP
KNBQ BNR
""")
    game
      .playMoves(A7 -> A8)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("a8=Q#")))
  test("castle kingside"):
    Game("""
PP   PPP
R   K  R
""").playMoves(E1 -> G1)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("O-O")))
  test("castle queenside"):
    Game("""
PP   PPP
R   K  R
""").playMoves(E1 -> C1)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("O-O-O")))

  test("ambiguous file only"):
    val game = Game("""
k





P   K  P
R      R
""")
    game
      .playMoves(H1 -> B1)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("Rhb1")))
  test("ambiguous rank only"):
    val game = Game("""
k


 N


    K  P
 N
""")
    game
      .playMoves(B5 -> C3)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("N5c3")))
  test("ambiguous file and rank"):
    val game = Game("""


  QQ
  Q


    K
k
""")
    game
      .playMoves(C6 -> D5)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("Qc6d5")))
  test("unambiguous file"):
    val game = Game("""
k





P      P
R   K  R
""")
    game
      .playMoves(H1 -> F1)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("Rf1")))
  test("unambiguous rank"):
    val game = Game("""
k

   KRq

    R



""")
    game
      .playMoves(E4 -> E5)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("Re5")))

  test("chess960 castle queenside as white"):
    Game(
      makeBoard(
        """
PPPPPPPP
NRK RQBB
""",
        variant.Chess960
      )
    ).playMoves(C1 -> B1)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("O-O-O")))
  test("chess960 castle kingside as white"):
    Game(
      makeBoard(
        """
PP PPPPP
NRK R  B
""",
        variant.Chess960
      )
    ).playMoves(C1 -> E1)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("O-O")))
  test("chess960 castle queenside as black"):
    Game(
      makeBoard(
        """
nrk rqbb
pppppppp




PPPPPPPP
NRK RQBB
""",
        variant.Chess960
      )
    ).withPlayer(Black)
      .playMoves(C8 -> B8)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("O-O-O")))
  test("chess960 castle kingside as black"):
    Game(
      makeBoard(
        """
nrk r  b
pppppppp




PPPPPPPP
NRK RQBB
""",
        variant.Chess960
      )
    ).withPlayer(Black)
      .playMoves(C8 -> E8)
      .map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, Vector(SanStr("O-O")))

  test("chess960 opening with castles"):
    Game(
      makeBoard(
        """
nrknrqbb
pppppppp




PPPPPPPP
NRKNRQBB
""",
        variant.Chess960
      )
    ).playMoves(
      F2 -> F4,
      D8 -> C6,
      D1 -> C3,
      G7 -> G6,
      C3 -> B5,
      C8 -> B8,
      C1 -> B1
    ).map(_.sans)
      .assertRight: ms =>
        assertEquals(ms, SanStr.from("f4 Nc6 Nc3 g6 Nb5 O-O-O O-O-O".split(' ').toVector))

  test("chess960 tricky rook disambiguation"):
    val fen           = EpdFen("r5k1/1b5p/N3p1p1/Q4p2/4r3/2P1q3/1PK2RP1/5R2 w - - 1 38")
    val sit           = Fen.read(fen).get
    val game1         = Game(sit.board, sit.color)
    val (game2, move) = game1(Square.F2, Square.F3).get
    assertEquals(Dumper(game1.situation, move, game2.situation), "Rf3")
