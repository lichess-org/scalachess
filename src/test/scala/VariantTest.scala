package chess

import cats.syntax.option.*
import chess.Square.*
import chess.format.{EpdFen, Fen}
import chess.variant.*
import org.specs2.specification.core.Fragment

class VariantTest extends ChessTest:

  val board = makeBoard

  "variants" should:
    "validate situation correctly" in:
      Fragment.foreach(List(Standard, Chess960, ThreeCheck, KingOfTheHill, Crazyhouse, Atomic)) { variant =>
        s"for variant $variant" in:
          "two-step pawn advance with no check should be valid" in:
            val position = EpdFen("2r3k1/p2Q1pp1/1p5p/3p4/P7/KP6/2r5/8 b - - 1 36")
            val game     = fenToGame(position, variant).flatMap(_.playMoves(A7 -> A5)).toOption.get
            game.situation.playable(true) must beTrue

          "when previous move is a double pawn push and checker is not the pushed pawn or a sliding piece" in:
            val game1 = Fen
              .read(variant, EpdFen("r1bqkbnr/1p1p1ppp/p7/2pPp3/4P3/5n2/PPP2PPP/RNBQKBNR w KQkq c6 0 4"))
              .get
            val game2 = Fen
              .read(variant, EpdFen("r1bqkbnr/1p1p1ppp/p7/2pP4/4P3/8/PPP2pPP/RNBQKBNR w KQkq c6 0 4"))
              .get

            game1.variant.valid(game1, true) must beFalse
            game1.variant.valid(game1, false) must beTrue
            game2.variant.valid(game2, true) must beFalse
            game2.variant.valid(game2, false) must beTrue

          "when previous move is a double pawn push and the only checker is a rook but not discovered check" in:
            val game = Fen
              .read(variant, EpdFen("1k6/5K1r/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HA c6 0 4"))
              .get
            game.variant.valid(game, true) must beFalse
            game.variant.valid(game, false) must beTrue

          "when previous move is a double pawn push and the only checker is a bishop but not discovered check" in:
            val game = Fen
              .read(variant, EpdFen("2b4r/kr5p/p7/2pP2b1/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4"))
              .get
            game.variant.valid(game, true) must beFalse
            game.variant.valid(game, false) must beTrue

          "when multiple checkers are aligned with the king" in:
            val game = Fen
              .read(variant, EpdFen("1nbqk3/1p1prppp/p1P5/8/4K3/8/PPP1rPPP/RNBQ1BNR w HA - 0 4"))
              .get
            game.variant.valid(game, true) must beFalse
            game.variant.valid(game, false) must beTrue

          "when previous move is a double pawn push and the only checker is the pushed pawn" in:
            val game = Fen
              .read(variant, EpdFen("r1bqkbnr/1p1p1ppp/p7/2pP4/3KP3/8/PPP3PP/RNBQ1BNR w HAkq c6 0 4"))
              .get
            game.variant.valid(game, true) must beTrue
            game.variant.valid(game, false) must beTrue

          "when two checkers are not on the same rank, file or diagonal" in:
            val game = Fen
              .read(variant, EpdFen("rnbqk2r/1p1p1ppp/p1P5/3np1b1/4P3/4K3/PPP2PPP/RNBQ1BNR w HAkq - 0 4"))
              .get
            game.variant.valid(game, true) must beTrue
            game.variant.valid(game, false) must beTrue

          "when previous move is a double pawn push and the only checker is a discovered rook check" in:
            val game = Fen
              .read(variant, EpdFen("1kb2b1r/1r3K1p/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HAk c6 0 4"))
              .get
            game.variant.valid(game, true) must beTrue
            game.variant.valid(game, false) must beTrue

          "when previous move is a double pawn push and the only checker is a discovered bishop check" in:
            val game = Fen
              .read(variant, EpdFen("1bb4r/kr5p/p7/2pP4/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4"))
              .get
            game.variant.valid(game, true) must beTrue
            game.variant.valid(game, false) must beTrue
      }

  "standard" should:

    "position pieces correctly" in:
      Standard.pieces must havePairs(
        A1 -> (White - Rook),
        B1 -> (White - Knight),
        C1 -> (White - Bishop),
        D1 -> (White - Queen),
        E1 -> (White - King),
        F1 -> (White - Bishop),
        G1 -> (White - Knight),
        H1 -> (White - Rook),
        A2 -> (White - Pawn),
        B2 -> (White - Pawn),
        C2 -> (White - Pawn),
        D2 -> (White - Pawn),
        E2 -> (White - Pawn),
        F2 -> (White - Pawn),
        G2 -> (White - Pawn),
        H2 -> (White - Pawn),
        A7 -> (Black - Pawn),
        B7 -> (Black - Pawn),
        C7 -> (Black - Pawn),
        D7 -> (Black - Pawn),
        E7 -> (Black - Pawn),
        F7 -> (Black - Pawn),
        G7 -> (Black - Pawn),
        H7 -> (Black - Pawn),
        A8 -> (Black - Rook),
        B8 -> (Black - Knight),
        C8 -> (Black - Bishop),
        D8 -> (Black - Queen),
        E8 -> (Black - King),
        F8 -> (Black - Bishop),
        G8 -> (Black - Knight),
        H8 -> (Black - Rook)
      )

    "Identify insufficient mating material when called (bishop)." in:
      val position = EpdFen("krq5/bqqq4/qqr5/1qq5/8/8/8/3qB2K b - -")
      val game     = fenToGame(position, Standard)

      game should beRight.like { case game =>
        game.board.materialImbalance must_== -91
        game.situation.opponentHasInsufficientMaterial must beTrue
      }

    "Identify sufficient mating material when called (bishop)." in:
      val position = EpdFen("8/7B/K7/2b5/1k6/8/8/8 b - -")
      val game     = fenToGame(position, Standard)

      game should beRight.like { case game =>
        game.board.materialImbalance must_== 0
        game.situation.opponentHasInsufficientMaterial must beFalse
      }

    "Identify insufficient mating material when called (knight)." in:
      val position = EpdFen("8/3k4/2q5/8/8/K1N5/8/8 b - -")
      val game     = fenToGame(position, Standard)

      game should beRight.like { case game =>
        game.board.materialImbalance must_== -6
        game.situation.opponentHasInsufficientMaterial must beTrue
      }

  "chess960" should:

    "position pieces correctly" in:
      Chess960.pieces must havePair(A2 -> (White - Pawn))

    "initialize the board with castling rights" in:
      Board.init(Chess960).history.castles must_== Castles.init

  "kingOfTheHill" should:
    "detect win" in:
      "not" in:
        Game(
          """
PPk
K
""".kingOfTheHill,
          White
        ).situation.end must beFalse
      "regular checkMate" in:
        val game = Game(
          """
PP
K  r
""".kingOfTheHill,
          White
        )

        game.situation.end must beTrue
        game.situation.winner must beSome { (_: Color) == Black }
      "centered black king" in:
        val sit = Game(
          """
   k

PP
   K
""".kingOfTheHill,
          White
        ).situation
        sit.end must beTrue
        sit.winner must beSome { (_: Color) == Black }

    "initialize the board with castling rights" in:
      Board.init(KingOfTheHill).history.castles must_== Castles.init

  "threeCheck" should:
    "detect win" in:
      "not" in:
        Game(
          """
PPk
K
""".threeCheck,
          White
        ).situation.end must beFalse
      "regular checkMate" in:
        val game = Game(
          """
PP
K  r
""".threeCheck,
          White
        )
        game.situation.end must beTrue
        game.situation.winner must beSome { (_: Color) == Black }
      "1 check" in:
        val game = Game(Board init ThreeCheck)
          .playMoves(
            E2 -> E4,
            E7 -> E6,
            D2 -> D4,
            F8 -> B4
          )
          .toOption
          .get
        game.situation.end must beFalse
      "2 checks" in:
        val game = Game(Board init ThreeCheck)
          .playMoves(
            E2 -> E4,
            E7 -> E6,
            D2 -> D4,
            F8 -> B4,
            C2 -> C3,
            B4 -> C3
          )
          .toOption
          .get
        game.situation.end must beFalse
      "3 checks" in:
        val game = Game(Board init ThreeCheck)
          .playMoves(
            E2 -> E4,
            E7 -> E6,
            D2 -> D4,
            F8 -> B4,
            C2 -> C3,
            B4 -> C3,
            B1 -> C3,
            D8 -> H4,
            A2 -> A3,
            H4 -> F2
          )
          .toOption
          .get
        game.situation.end must beTrue

        game.situation.winner must beSome { (_: Color) == Black }

    "Not force a draw when there is insufficient mating material" in:
      val position = EpdFen("8/6K1/8/8/8/8/k6p/8 b - - 1 39")
      val game     = fenToGame(position, ThreeCheck)

      val successGame = game flatMap (_.playMove(Square.H2, Square.H1, Knight.some))

      successGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    "Force a draw when there are only kings remaining" in:
      val position = EpdFen("8/6K1/8/8/8/8/k7/8 b - -")
      val game     = fenToGame(position, ThreeCheck)

      game must beRight.like { case game =>
        game.situation.end must beTrue
        game.situation.status must beEqualTo(Status.Draw.some)
      }

    "initialize the board with castling rights" in:
      Board.init(KingOfTheHill).history.castles must_== Castles.init

  "racingKings" should:
    "call it stalemate when there is no legal move" in:
      val position = EpdFen("8/8/8/8/3K4/8/1k6/b7 b - - 5 3")
      val game     = fenToGame(position, RacingKings)

      game must beRight.like { case game =>
        game.situation.end must beTrue
        game.situation.staleMate must beTrue
      }

    "should not draw because of insufficient material" in:
      val position = EpdFen("8/8/8/8/5K2/8/2k5/8 w - - 0 1")
      val game     = fenToGame(position, RacingKings)

      game must beRight.like { case game =>
        game.situation.end must beFalse
        game.situation.staleMate must beFalse
      }

    "should recognize a king in the goal" in:
      "white" in:
        val position = EpdFen("2K5/8/6k1/8/8/8/8/Q6q w - - 0 1")
        val game     = fenToGame(position, RacingKings)

        game must beRight { (game: Game) =>
          game.situation.end must beTrue
          game.situation.winner must beSome { (_: Color) == White }
        }

      "black" in:
        val position = EpdFen("6k1/8/8/8/8/2r5/1KB5/2B5 w - - 0 1")
        val game     = fenToGame(position, RacingKings)

        game must beRight { (game: Game) =>
          game.situation.end must beTrue
          game.situation.winner must beSome { (_: Color) == Black }
        }

    "should give black one more move" in:
      "when white is in the goal" in:
        val position = EpdFen("2K5/5k2/8/8/8/8/8/8 b - - 0 1")
        val game     = fenToGame(position, RacingKings)

        game must beRight.like { case game =>
          game.situation.end must beFalse
        }

      "but not if it does not matter anyway" in:
        val position = EpdFen("2K5/8/2n1nk2/8/8/8/8/4r3 b - - 0 1")
        val game     = fenToGame(position, RacingKings)

        game must beRight { (game: Game) =>
          game.situation.end must beTrue
          game.situation.winner must beSome:
            (_: Color) == White
        }

    "should call it a draw with both kings in the goal" in:
      val position = EpdFen("2K2k2/8/8/8/8/1b6/1b6/8 w - - 0 1")
      val game     = fenToGame(position, RacingKings)

      game must beRight.like { case game =>
        game.situation.end must beTrue
        game.situation.status must beEqualTo(Status.Draw.some)
      }

    "initialize the board without castling rights" in:
      Board.init(RacingKings).history.castles.isEmpty must beTrue

    "validate situation correctly" in :
      Fragment.foreach(
        List(
          "1bb4r/kr5p/p7/2pP4/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4",
          "1kb2b1r/1r3K1p/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HAk c6 0 4",
          "rnbqk2r/1p1p1ppp/p1P5/3np1b1/4P3/4K3/PPP2PPP/RNBQ1BNR w HAkq - 0 4",
          "r1bqkbnr/1p1p1ppp/p7/2pP4/3KP3/8/PPP3PP/RNBQ1BNR w HAkq c6 0 4",
          "1nbqk3/1p1prppp/p1P5/8/4K3/8/PPP1rPPP/RNBQ1BNR w HA - 0 4",
          "2b4r/kr5p/p7/2pP2b1/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4",
          "1k6/5K1r/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HA c6 0 4",
          "r1bqkbnr/1p1p1ppp/p7/2pP4/4P3/8/PPP2pPP/RNBQKBNR w KQkq c6 0 4",
          "r1bqkbnr/1p1p1ppp/p7/2pPp3/4P3/5n2/PPP2PPP/RNBQKBNR w KQkq c6 0 4"
        )
      ) { fen =>
        s"for fen $fen" in :
          val game = Fen
            .read(RacingKings, EpdFen(fen))
            .get
          game.variant.valid(game, true) must beFalse
          game.variant.valid(game, false) must beTrue
      }

  "antichess" should:
    "initialize the board without castling rights" in:
      Board.init(Antichess).history.castles.isEmpty must beTrue

    "calculate material imbalance" in:
      val position = EpdFen("8/p7/8/8/2B5/b7/PPPK2PP/RNB3NR w - - 1 16")
      val game     = fenToGame(position, Antichess)

      game must beRight.like { case game =>
        game.situation.board.materialImbalance must_== -20
      }

    "validate situation correctly" in:
      Fragment.foreach(
        List(
          "1bb4r/kr5p/p7/2pP4/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4",
          "1kb2b1r/1r3K1p/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HAk c6 0 4",
          "rnbqk2r/1p1p1ppp/p1P5/3np1b1/4P3/4K3/PPP2PPP/RNBQ1BNR w HAkq - 0 4",
          "r1bqkbnr/1p1p1ppp/p7/2pP4/3KP3/8/PPP3PP/RNBQ1BNR w HAkq c6 0 4",
          "1nbqk3/1p1prppp/p1P5/8/4K3/8/PPP1rPPP/RNBQ1BNR w HA - 0 4",
          "2b4r/kr5p/p7/2pP2b1/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4",
          "1k6/5K1r/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HA c6 0 4",
          "r1bqkbnr/1p1p1ppp/p7/2pP4/4P3/8/PPP2pPP/RNBQKBNR w KQkq c6 0 4",
          "r1bqkbnr/1p1p1ppp/p7/2pPp3/4P3/5n2/PPP2PPP/RNBQKBNR w KQkq c6 0 4"
        )
      ) { fen =>
        s"for fen $fen" in:
          val game = Fen
            .read(Antichess, EpdFen(fen))
            .get
          game.variant.valid(game, true) must beTrue
          game.variant.valid(game, false) must beTrue
      }
  "horde" should:
    "initialize the board with black castling rights" in:
      Board.init(Horde).history.castles must_== Castles("kq")

  "racing king" should:
    "validate situation correctly" in:
      "with any check at all for white" in:
        val position = EpdFen("8/8/8/k5R1/8/8/1rbnNB1K/qrbnNBRQ b - - 0 1")
        val game     = fenToGame(position, RacingKings).toOption.get
        game.situation.playable(true) must beFalse
        game.situation.playable(false) must beTrue

      "with any check at all for black" in:
        val position = EpdFen("8/8/8/k7/7r/8/2bnNBRK/qrbnNBRQ w - - 0 1")
        val game     = fenToGame(position, RacingKings).toOption.get
        game.situation.playable(true) must beFalse
        game.situation.playable(false) must beTrue

  "atomic chess" should:
    "validate situation correctly" in:
      "kings next to each other and there multiple checkers" in:
        val position = EpdFen("8/8/4N3/1Rk4p/1B1K4/8/8/8 b - - 1 6")
        val game     = fenToGame(position, Atomic).toOption.get
        game.situation.playable(true) must beTrue
        game.situation.playable(false) must beTrue

  "horde" should:
    "validate situation correctly" in:
      "two-step pawn advance with no check should be valid" in:
        val position = EpdFen("2r3k1/p2P1pp1/1p5p/3p4/P7/PP6/2P3P1/8 b - - 1 36")
        val game     = fenToGame(position, Horde).flatMap(_.playMoves(A7 -> A5)).toOption.get
        game.situation.playable(true) must beTrue

      "when previous move is a double pawn push and checker is not the pushed pawn or a sliding piece" in:
        val game = Fen
          .read(Horde, EpdFen("1r6/6q1/8/3k4/2pPP3/8/PPP2PPP/PPPPPPPP b - d3 0 1"))
          .get
        game.variant.valid(game, false) must beTrue
        game.variant.valid(game, true) must beFalse

      "when previous move is a double pawn push and the only checker is a rook but not discovered check" in:
        val game = Fen
          .read(
            Horde,
            EpdFen("5r2/8/4k2R/8/3pP3/8/PPPP1PPP/2PPPPP1 b - e3 0 1")
          )
          .get
        game.variant.valid(game, true) must beFalse
        game.variant.valid(game, false) must beTrue

      "when previous move is a double pawn push and the only checker is a bishop but not discovered check" in:
        val game = Fen
          .read(Horde, EpdFen("5r2/8/4k3/8/3pP1B1/8/PPPP1PPP/2PPPPP1 b - e3 0 1"))
          .get
        game.variant.valid(game, true) must beFalse
        game.variant.valid(game, false) must beTrue

      "when multiple checkers are aligned with the king" in:
        val game = Fen
          .read(Horde, EpdFen("1q6/8/R2k1R2/8/8/8/8/8 b - - 0 1"))
          .get
        game.variant.valid(game, true) must beFalse
        game.variant.valid(game, false) must beTrue

      "when previous move is a double pawn push and the only checker is the pushed pawn" in:
        val game = Fen
          .read(Horde, EpdFen("1r6/6q1/8/4k3/2pP4/2P5/PP3PPP/PPPPPPPP b - d3 0 3"))
          .get
        game.variant.valid(game, true) must beTrue
        game.variant.valid(game, false) must beTrue

      "when two checkers are not on the same rank, file or diagonal" in:
        val game = Fen
          .read(Horde, EpdFen("7r/3k4/8/1B2N2q/1B6/8/PPPPPPPP/PPPPPPPP w - - 0 1"))
          .get
        game.variant.valid(game, true) must beTrue
        game.variant.valid(game, false) must beTrue

      "when previous move is a double pawn push and the only checker is a discovered rook check" in:
        val game = Fen
          .read(Horde, EpdFen("8/8/8/8/3Pp3/8/k5R1/8 b - d3 0 2"))
          .get
        game.variant.valid(game, true) must beTrue
        game.variant.valid(game, false) must beTrue

      "when previous move is a double pawn push and the only checker is a discovered bishop check" in:
        val game = Fen
          .read(Horde, EpdFen("8/8/8/8/1k1Pp3/8/8/4B3 b - d3 0 2"))
          .get
        game.variant.valid(game, true) must beTrue
        game.variant.valid(game, false) must beTrue
