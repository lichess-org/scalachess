package chess

import cats.syntax.option.*
import chess.format.pgn.Reader
import chess.format.{ Fen, FullFen }
import chess.variant.Antichess

import scala.language.implicitConversions

class AntichessVariantTest extends ChessTest:

  // Random PGN taken from FICS
  val fullGame =
    """[Event "3 0 rated antichess"]
[Site "freechess.org"]
[Date "2014.12.12"]
[Round "?"]
[White "Gnarus"]
[Black "CatNail"]
[Result "0-1"]
[ResultDescription "CatNail wins by losing all material"]
[WhiteElo "1778"]
[BlackElo "2025"]
[PlyCount "67"]
[Variant "antichess"]
[TimeControl "180+0"]
[WhiteClock "00:03:00.0"]
[BlackClock "00:03:00.0"]
[WhiteLagMillis "1470"]
[BlackLagMillis "0"]
[WhiteRemainingMillis "139297"]
[BlackRemainingMillis "173163"]
[WhiteOnTop "0"]

1. g3 {[%emt 0.000]} h6 {[%emt 0.000]} 2. Nh3 {[%emt 1.156]} a6 {[%emt 0.221]} 3. Ng5 {[%emt 0.454]}
hxg5 {[%emt 0.220]} 4. Bh3 {[%emt 0.671]} Rxh3 {[%emt 0.221]} 5. Rg1 {[%emt 0.516]}
Rxg3 {[%emt 0.201]} 6. hxg3 {[%emt 0.578]} b6 {[%emt 0.201]} 7. Na3 {[%emt 1.204]}
g6 {[%emt 0.225]} 8. Nb5 {[%emt 0.436]} axb5 {[%emt 0.219]} 9. a4 {[%emt 0.735]}
Rxa4 {[%emt 0.206]} 10. Rxa4 {[%emt 0.875]} bxa4 {[%emt 0.221]} 11. b3 {[%emt 0.296]}
axb3 {[%emt 0.201]} 12. cxb3 {[%emt 0.109]} Nh6 {[%emt 0.200]} 13. Ba3 {[%emt 0.656]}
Na6 {[%emt 0.221]} 14. Bxe7 {[%emt 0.703]} Bxe7 {[%emt 0.223]} 15. b4 {[%emt 0.656]}
Bxb4 {[%emt 0.200]} 16. g4 {[%emt 5.594]} Bxd2 {[%emt 0.201]} 17. Qxd2 {[%emt 0.906]}
Nxg4 {[%emt 0.199]} 18. Qxg5 {[%emt 0.907]} Nxf2 {[%emt 0.221]} 19. Qxd8 {[%emt 4.313]}
Kxd8 {[%emt 0.220]} 20. Rxg6 {[%emt 1.718]} fxg6 {[%emt 0.221]} 21. Kxf2 {[%emt 0.767]}
c5 {[%emt 0.200]} 22. Kf3 {[%emt 1.594]} Ke8 {[%emt 0.201]} 23. e4 {[%emt 0.939]}
Nb8 {[%emt 0.201]} 24. e5 {[%emt 0.484]} d5 {[%emt 0.201]} 25. exd6 {[%emt 0.437]}
Nd7 {[%emt 0.201]} 26. Ke3 {[%emt 3.984]} Ke7 {[%emt 0.201]} 27. dxe7 {[%emt 1.422]}
g5 {[%emt 0.200]} 28. Kd4 {[%emt 1.172]} cxd4 {[%emt 0.200]} 29. e8=R {[%emt 4.329]}
Ne5 {[%emt 0.364]} 30. Rxc8 {[%emt 0.469]} Nc4 {[%emt 0.201]} 31. Rxc4 {[%emt 1.592]}
b5 {[%emt 0.223]} 32. Rxd4 {[%emt 0.359]} b4 {[%emt 0.202]} 33. Rxb4 {[%emt 0.500]}
g4 {[%emt 0.200]} 34. Rxg4 {[%emt 0.172]} 0-1"""

  test("Allow an opening move for white taking into account a player may move without taking if possible"):
    val startingPosition = Game(Antichess)
    val newGame          = startingPosition.playMove(Square.E2, Square.E4, None).get
    val fen              = Fen.write(newGame)
    assertEquals(fen, FullFen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b - - 0 1"))

  test("Not allow a player to make a non capturing move if a capturing move is available"):
    val game             = Game(Antichess)
    val gameAfterOpening = game.playMoves((Square.E2, Square.E4), (Square.F7, Square.F5))
    val invalidGame      = gameAfterOpening.flatMap(_.playMove(Square.H2, Square.H4))
    assertEquals(invalidGame, Left(ErrorStr("Piece on h2 cannot move to h4")))

  test("A situation in antichess should only present the capturing moves if the player can capture"):
    val game = Game(Antichess).playMoves((Square.E2, Square.E4), (Square.F7, Square.F5)).get
    assertEquals(game.situation.legalMoves.size, 1)
    assertNot(game.situation.legalMoves.exists(_.captures == false))

  test("Allow a capturing move to be made"):
    val game =
      Game(Antichess).playMoves((Square.E2, Square.E4), (Square.F7, Square.F5), (Square.E4, Square.F5))
    assert(game.isRight)

  test("Not permit a player to castle"):
    // Castling is not allowed in antichess
    val game = Game(Antichess).playMoves(
      (Square.E2, Square.E4),
      (Square.E7, Square.E5),
      (Square.F1, Square.E2),
      (Square.G8, Square.H6),
      (Square.G1, Square.H3)
    )
    val possibleDestinations =
      game.flatMap(_.board.destsFrom(Square.E1).toRight("king has no destinations"))
    assertEquals(possibleDestinations.get, List(Square.F1))

  test("Not allow a king to be put into check"):
    val game = Game(Antichess)
      .playMoves(
        Square.E2 -> Square.E4,
        Square.E7 -> Square.E5,
        Square.D1 -> Square.H5
      )
      .get
    assertEquals(game.situation.check, Check.No)

  test("Allow kings to be captured"):
    val game = Game(Antichess)
      .playMoves(
        Square.E2 -> Square.E4,
        Square.E7 -> Square.E5,
        Square.D1 -> Square.H5,
        Square.F7 -> Square.F6,
        Square.H5 -> Square.E8
      )
      .get
    assert(game.board.kingOf(Color.black).isEmpty)

  test("Not allow a king to be check mated"):
    val game = Game(Antichess)
      .playMoves(
        Square.F2 -> Square.F3,
        Square.E7 -> Square.E6,
        Square.G2 -> Square.G4,
        Square.D8 -> Square.H4
      )
      .get
    assertNot(game.situation.checkMate)

  test("Allow a pawn to be promoted to a king"):
    val position = FullFen("8/5P2/8/2b5/8/8/4B3/8 w - -")
    val game     = fenToGame(position, Antichess)
    val newGame  = game(Square.F7, Square.F8, Option(King)).get._1
    assertEquals(newGame.board(Square.F8), Option(White - King))

  test("deal with 2 white kings"):
    val position = FullFen("K3k1nr/p2q2pp/p2p1p2/8/2PP4/8/PP4PP/RNBQK1NR w - - 0 11")
    val game     = fenToGame(position, Antichess)
    assertEquals(game.situation.destinations, Map(Square.A8 -> Square.A7.bb))

  test("Be drawn when there are only opposite colour bishops remaining"):
    val position = FullFen("8/2b5/8/8/8/6Q1/4B3/8 b - -")
    val game     = fenToGame(position, Antichess)(Square.C7, Square.G3, None).get._1
    assert(game.situation.end)
    assert(game.situation.autoDraw)
    assertEquals(game.situation.winner, None)
    assertEquals(game.situation.status, Some(Status.Draw))

  test("Be drawn on multiple bishops on the opposite color"):
    val position = FullFen("8/6P1/8/8/1b6/8/8/5B2 w - -")
    val game     = fenToGame(position, Antichess)(Square.G7, Square.G8, Bishop.some).get._1
    assert(game.situation.end)
    assert(game.situation.autoDraw)
    assertEquals(game.situation.winner, None)
    assertEquals(game.situation.status, Some(Status.Draw))

  test("Not be drawn when the black and white bishops are on the same coloured squares "):
    val position = FullFen("7b/8/1p6/8/8/8/5B2/8 w - -")
    val game     = fenToGame(position, Antichess)(Square.F2, Square.B6, None).get._1
    assertNot(game.situation.end)
    assertNot(game.situation.autoDraw)
    assertEquals(game.situation.winner, None)

  test(
    "Be drawn when there are only opposite colour bishops and pawns which could not attack those bishops remaining"
  ):
    val position = FullFen("8/6p1/4B1P1/4p3/4P3/8/2p5/8 b - - 1 28")
    val game     = fenToGame(position, Antichess)(Square.C2, Square.C1, Option(Bishop)).get._1
    assert(game.situation.end)
    assert(game.situation.autoDraw)
    assertEquals(game.situation.status, Some(Status.Draw))

  test("Not be drawn on opposite color bishops but with pawns that could be forced to attack a bishop"):
    val position = FullFen("8/6p1/1B4P1/4p3/4P3/8/3p4/8 b - -")
    val game     = fenToGame(position, Antichess)(Square.D2, Square.D1, Option(Bishop)).get._1
    assertNot(game.situation.end)
    assertNot(game.situation.autoDraw)
    assertEquals(game.situation.winner, None)

  test("Not be drawn where a white bishop can attack a black pawn in an almost closed position"):
    val position = FullFen("5b2/1P4p1/4B1P1/4p3/4P3/8/8/8 w - -")
    val game     = fenToGame(position, Antichess)(Square.B7, Square.B8, Bishop.some).get._1
    assertNot(game.situation.end)
    assertNot(game.situation.autoDraw)
    assertEquals(game.situation.winner, None)

  test("Not be drawn where a pawn is unattackable, but is blocked by a bishop, not a pawn"):
    val position = FullFen("8/8/4BbP1/4p3/4P3/8/8/8 b - -")
    val game     = fenToGame(position, Antichess).playMoves(Square.F6 -> Square.G7).get
    assertNot(game.situation.end)
    assertNot(game.situation.autoDraw)
    assertEquals(game.situation.status, None)

  test("Opponent has insufficient material when there are only two remaining knights on same color squares"):
    val position = FullFen("8/8/3n2N1/8/8/8/8/8 w - -")
    val game     = fenToGame(position, Antichess).playMoves(Square.G6 -> Square.F4).get
    assert(game.situation.opponentHasInsufficientMaterial)

  test(
    "Opponent has sufficient material when there are only two remaining knights on opposite color squares"
  ):
    val position = FullFen("7n/8/8/8/8/8/8/N7 w - -")
    val game     = fenToGame(position, Antichess).playMoves(Square.A1 -> Square.B3).get
    assertNot(game.situation.opponentHasInsufficientMaterial)

  test("Not be drawn on insufficient mating material"):
    val position = FullFen("4K3/8/1b6/8/8/8/5B2/3k4 b - -")
    val game     = fenToGame(position, Antichess)
    assertNot(game.situation.end)

  test("Be drawn on a three move repetition"):
    val game = Game(Antichess)
    val moves =
      List((Square.G1, Square.F3), (Square.G8, Square.F6), (Square.F3, Square.G1), (Square.F6, Square.G8))
    val repeatedMoves: List[(Square, Square)] = List.fill(3)(moves).flatten
    val g                                     = game.playMoveList(repeatedMoves).get
    assert(g.situation.threefoldRepetition)

  test("Successfully play through a full game until one player loses all their pieces"):
    Reader
      .full(fullGame)
      .assertRight:
        case Reader.Result(replay, None) =>
          val game = replay.state
          assert(game.situation.end)
          // In antichess, the player who has just lost all their pieces is the winner
          assertEquals(game.situation.winner, Some(Black))

  test("Win on a traditional stalemate where the player has no valid moves"):
    val position = FullFen("8/p7/8/P7/8/8/8/8 w - -")
    val game     = fenToGame(position, Antichess).playMoves(Square.A5 -> Square.A6).get
    assert(game.situation.end)
    assertEquals(game.situation.winner, Some(Black))

  test("Stalemate is a win - second test"):
    val fen  = FullFen("2Q5/8/p7/8/8/8/6PR/8 w - -")
    val game = fenToGame(fen, Antichess).playMoves(Square.C8 -> Square.A6).get
    assert(game.situation.end)
    assertEquals(game.situation.status, Some(Status.VariantEnd))
    assertEquals(game.situation.winner, Some(Black))

  test("two kings on replay"):
    val pgn = """
[Event "Rated Antichess game"]
[Site "https://lichess.org/60a0EiZh"]
[Date "2023.01.16"]
[White "ArtemiyG"]
[Black "ashok20"]
[Result "1/2-1/2"]
[UTCDate "2023.01.16"]
[UTCTime "10:29:03"]
[WhiteElo "1249"]
[BlackElo "1688"]
[WhiteRatingDiff "+10"]
[BlackRatingDiff "-5"]
[Variant "Antichess"]
[TimeControl "300+0"]
[ECO "?"]
[Opening "?"]
[Termination "Normal"]
[Annotator "lichess.org"]

1. e3 b5 2. Bxb5 Bb7 3. Bxd7 Bxg2 4. Bxe8 Bxh1 5. Bxf7 Qxd2 6. Bxg8 Qxc2 7. Bxh7 Rxh7 8. Qxc2 Rxh2 9. Qxc7 Rxf2 10. Qxe7 Bxe7 11. Kxf2 Ba3 12. bxa3 Bf3 13. Nxf3 Nc6 14. Ne5 Nxe5 15. Kf3 Nxf3 16. Bd2 Nxd2 17. Nxd2 Rh8 18. Rh1 Rxh1 19. Nf1 Rxf1 20. a4 g6 21. e4 Rb1 22. a5 a6 23. a4 Rb6 24. axb6 g5 25. b7 g4 26. b8=R g3 27. a5 g2 28. Rb5 axb5 29. e5 g1=R 30. e6 b4 31. e7 Rg3 32. e8=K b3 33. a6 b2 34. a7 b1=R 35. a8=K Rb8 36. Kxb8 Rg6 37. Kc8 Ra6 38. Kf8 Rh6 39. Ke8 Rg6 40. Kcd8 Rb6 41. Kf8 Rh6 42. Kfe8 Rb6 43. Kf8 Rh6 44. Kfe8 Rb6 { The game is a draw. } 1/2-1/2
    """
    Reader
      .full(pgn)
      .assertRight:
        case Reader.Result(replay, None) =>
          val game = replay.state
          assertNot(game.situation.end)
          assertEquals(game.situation.winner, None)

  test("fen with castles"):
    val game = fenToGame(FullFen("rnbqk2r/ppppppbp/5np1/8/8/5NP1/PPPPPPBP/RNBQK2R w KQkq - 4 4"), Antichess)
    assertEquals(game.situation.board.history.castles, Castles.none)
    assertEquals(game.situation.board.history.unmovedRooks, UnmovedRooks.none)
