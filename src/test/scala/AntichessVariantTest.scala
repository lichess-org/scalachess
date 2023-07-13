package chess

import scala.language.implicitConversions
import cats.syntax.option.*

import chess.format.{ EpdFen, Fen }
import chess.format.pgn.Reader
import chess.variant.Antichess

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

  "Antichess " should:

    "Allow an opening move for white taking into account a player may move without taking if possible" in:
      val startingPosition = Game(Antichess)
      val afterFirstMove   = startingPosition.playMove(Square.E2, Square.E4, None)

      afterFirstMove must beRight.like { newGame =>
        val fen = Fen write newGame
        fen mustEqual EpdFen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b - - 0 1")
      }

    "Not allow a player to make a non capturing move if a capturing move is available" in:
      val game             = Game(Antichess)
      val gameAfterOpening = game.playMoves((Square.E2, Square.E4), (Square.F7, Square.F5))

      val invalidGame = gameAfterOpening flatMap (_.playMove(Square.H2, Square.H4))

      invalidGame must beLeft("Piece on h2 cannot move to h4")

    "A situation in antichess should only present the capturing moves if the player can capture" in:
      val game             = Game(Antichess)
      val gameAfterOpening = game.playMoves((Square.E2, Square.E4), (Square.F7, Square.F5))

      gameAfterOpening must beRight.like { case newGame =>
        newGame.situation.legalMoves.size must beEqualTo(1)
        newGame.situation.legalMoves.exists(_.captures == false) must beEqualTo(false)
      }

    "Allow a capturing move to be made" in:
      val game =
        Game(Antichess).playMoves((Square.E2, Square.E4), (Square.F7, Square.F5), (Square.E4, Square.F5))
      game must beRight

    "Not permit a player to castle" in:
      // Castling is not allowed in antichess
      val game = Game(Antichess).playMoves(
        (Square.E2, Square.E4),
        (Square.E7, Square.E5),
        (Square.F1, Square.E2),
        (Square.G8, Square.H6),
        (Square.G1, Square.H3)
      )

      val possibleDestinations =
        game flatMap (_.board.destsFrom(Square.E1).toRight("king has no destinations"))

      possibleDestinations must beRight.like { case dests =>
        // G1 (to castle) should not be a valid destination
        dests must beEqualTo(List(Square.F1))
      }

    "Not allow a king to be put into check" in:
      val game = Game(Antichess).playMoves(
        Square.E2 -> Square.E4,
        Square.E7 -> Square.E5,
        Square.D1 -> Square.H5
      )

      game must beRight.like { newGame =>
        newGame.situation.check === Check.No
      }

    "Allow kings to be captured" in:
      val game = Game(Antichess).playMoves(
        Square.E2 -> Square.E4,
        Square.E7 -> Square.E5,
        Square.D1 -> Square.H5,
        Square.F7 -> Square.F6,
        Square.H5 -> Square.E8
      )

      game must beRight.like { case newGame =>
        newGame.board.kingOf(Color.black).isEmpty
      }

    "Not allow a king to be check mated" in:
      val game = Game(Antichess).playMoves(
        Square.F2 -> Square.F3,
        Square.E7 -> Square.E6,
        Square.G2 -> Square.G4,
        Square.D8 -> Square.H4
      )

      game must beRight.like { case newGame =>
        newGame.situation.checkMate must beFalse
      }

    "Allow a pawn to be promoted to a king" in:
      val position     = EpdFen("8/5P2/8/2b5/8/8/4B3/8 w - -")
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.apply(Square.F7, Square.F8, Option(King))) map (_._1)

      newGame must beRight:
        (_: Game).board(Square.F8).mustEqual(Option(White - King))

    "deal with 2 white kings" in:
      val position     = EpdFen("K3k1nr/p2q2pp/p2p1p2/8/2PP4/8/PP4PP/RNBQK1NR w - - 0 11")
      val originalGame = fenToGame(position, Antichess).toOption.get

      originalGame.situation.destinations === Map(Square.A8 -> List(Square.A7))

    "Be drawn when there are only opposite colour bishops remaining" in:
      val position     = EpdFen("8/2b5/8/8/8/6Q1/4B3/8 b - -")
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.apply(Square.C7, Square.G3, None)) map (_._1)

      newGame must beRight.like { case (drawnGame: Game) =>
        drawnGame.situation.end must beTrue
        drawnGame.situation.autoDraw must beTrue
        drawnGame.situation.winner must beNone
        drawnGame.situation.status must beSome(Status.Draw)
      }

    "Be drawn on multiple bishops on the opposite color" in:
      val position     = EpdFen("8/6P1/8/8/1b6/8/8/5B2 w - -")
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.apply(Square.G7, Square.G8, Bishop.some)) map (_._1)

      newGame must beRight.like { case (drawnGame: Game) =>
        drawnGame.situation.end must beTrue
        drawnGame.situation.autoDraw must beTrue
        drawnGame.situation.winner must beNone
        drawnGame.situation.status must beSome(Status.Draw)
      }

    "Not be drawn when the black and white bishops are on the same coloured squares " in:
      val position     = EpdFen("7b/8/1p6/8/8/8/5B2/8 w - -")
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.apply(Square.F2, Square.B6, None)) map (_._1)

      newGame must beRight.like { case nonDrawnGame =>
        nonDrawnGame.situation.end must beFalse
        nonDrawnGame.situation.autoDraw must beFalse
        nonDrawnGame.situation.winner must beNone
      }

    "Be drawn when there are only opposite colour bishops and pawns which could not attack those bishops remaining" in:
      val position     = EpdFen("8/6p1/4B1P1/4p3/4P3/8/2p5/8 b - - 1 28")
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.apply(Square.C2, Square.C1, Option(Bishop))) map (_._1)

      newGame must beRight.like { case (drawnGame: Game) =>
        drawnGame.situation.end must beTrue
        drawnGame.situation.autoDraw must beTrue
        drawnGame.situation.status must beSome(Status.Draw)
      }

    "Not be drawn on opposite color bishops but with pawns that could be forced to attack a bishop" in:
      val position     = EpdFen("8/6p1/1B4P1/4p3/4P3/8/3p4/8 b - -")
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.apply(Square.D2, Square.D1, Option(Bishop))) map (_._1)

      newGame must beRight.like { case nonDrawnGame =>
        nonDrawnGame.situation.end must beFalse
        nonDrawnGame.situation.autoDraw must beFalse
        nonDrawnGame.situation.status must beNone
      }

    "Not be drawn where a white bishop can attack a black pawn in an almost closed position" in:
      val position     = EpdFen("5b2/1P4p1/4B1P1/4p3/4P3/8/8/8 w - -")
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.apply(Square.B7, Square.B8, Bishop.some)) map (_._1)

      newGame must beRight.like { case nonDrawnGame =>
        nonDrawnGame.situation.end must beFalse
        nonDrawnGame.situation.autoDraw must beFalse
        nonDrawnGame.situation.status must beNone
      }

    "Not be drawn where a pawn is unattackable, but is blocked by a bishop, not a pawn" in:
      val position     = EpdFen("8/8/4BbP1/4p3/4P3/8/8/8 b - -")
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.playMoves(Square.F6 -> Square.G7))

      newGame must beRight.like { case nonDrawnGame =>
        nonDrawnGame.situation.end must beFalse
        nonDrawnGame.situation.autoDraw must beFalse
        nonDrawnGame.situation.status must beNone
      }

    "Opponent has insufficient material when there are only two remaining knights on same color squares" in:
      val position     = EpdFen("8/8/3n2N1/8/8/8/8/8 w - -")
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.playMoves(Square.G6 -> Square.F4))

      newGame must beRight.like:
        _.situation.opponentHasInsufficientMaterial must beTrue

    "Opponent has sufficient material when there are only two remaining knights on opposite color squares" in:
      val position     = EpdFen("7n/8/8/8/8/8/8/N7 w - -")
      val originalGame = fenToGame(position, Antichess)

      val newGame = originalGame flatMap (_.playMoves(Square.A1 -> Square.B3))

      newGame must beRight.like:
        _.situation.opponentHasInsufficientMaterial must beFalse

    "Not be drawn on insufficient mating material" in:
      val position  = EpdFen("4K3/8/1b6/8/8/8/5B2/3k4 b - -")
      val maybeGame = fenToGame(position, Antichess)

      maybeGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    "Be drawn on a three move repetition" in:
      val game = Game(Antichess)

      val moves =
        List((Square.G1, Square.F3), (Square.G8, Square.F6), (Square.F3, Square.G1), (Square.F6, Square.G8))
      val repeatedMoves: List[(Square, Square)] = List.fill(3)(moves).flatten

      val drawnGame = game.playMoveList(repeatedMoves)

      drawnGame must beRight.like { case g =>
        g.situation.threefoldRepetition must beTrue
      }

    "Successfully play through a full game until one player loses all their pieces" in:
      val game = Reader.full(fullGame)

      game must beRight.like { case Reader.Result.Complete(replay) =>
        val game = replay.state

        game.situation.end must beTrue

        // In antichess, the player who has just lost all their pieces is the winner
        game.situation.winner must beSome(Black)
      }

    "Win on a traditional stalemate where the player has no valid moves" in:
      val position  = EpdFen("8/p7/8/P7/8/8/8/8 w - -")
      val maybeGame = fenToGame(position, Antichess)

      val drawnGame = maybeGame flatMap (_.playMoves((Square.A5, Square.A6)))

      drawnGame must beRight.like { case game =>
        game.situation.end must beTrue
        game.situation.winner must beSome(Black)
      }

    "Stalemate is a win - second test" in:
      val fen       = EpdFen("2Q5/8/p7/8/8/8/6PR/8 w - -")
      val maybeGame = fenToGame(fen, Antichess)

      val drawnGame = maybeGame flatMap (_.playMoves((Square.C8, Square.A6)))

      drawnGame must beRight.like { case game =>
        game.situation.end must beTrue
        game.situation.status must beSome(Status.VariantEnd)
        game.situation.winner must beSome(Black)
      }

    "two kings on replay" in:

      val pgn  = """
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
      val game = Reader.full(pgn)

      game must beRight.like { case Reader.Result.Complete(replay) =>
        val game = replay.state

        game.situation.end must beFalse
        game.situation.winner must beNone
      }

    "fen with castles" in:
      val game = fenToGame(EpdFen("rnbqk2r/ppppppbp/5np1/8/8/5NP1/PPPPPPBP/RNBQK2R w KQkq - 4 4"), Antichess)

      game must beRight.like { case game =>
        game.situation.board.history.castles must_== Castles.none
        game.situation.board.history.unmovedRooks must_== UnmovedRooks.none
      }
