package chess

import chess.format.{ EpdFen, Fen }
import chess.variant.Standard

class SituationTest extends ChessTest:

  "a game" should:
    "detect check" should:
      "by rook" in:
        ("""
K  r
""" as White).check === Check.Yes
      "by knight" in:
        ("""
  n
K
""" as White).check === Check.Yes
      "by bishop" in:
        ("""
  b


     K
""" as White).check === Check.Yes
      "by pawn" in:
        ("""
    p
     K
""" as White).check === Check.Yes
      "not" in:
        ("""
   n
K
""" as White).check === Check.No
    "detect check mate" in:
      "by rook" in:
        ("""
PP
K  r
""" as White).checkMate must beTrue
      "by knight" in:
        ("""
PPn
KR
""" as White).checkMate must beTrue
      "not" in:
        ("""
  n
K
""" as White).checkMate must beFalse
    "stale mate" in:
      "stuck in a corner" in:
        ("""
prr
K
""" as White).staleMate must beTrue
      "not" in:
        ("""
  b
K
""" as White).staleMate must beFalse

    "Give the correct winner for a game" in:
      val game =
        """
PP
K  r
""" as White

      game.checkMate must beTrue
      game.winner must beSome { (_: Color) == Black }

    "Not give a winner if the game is still in progress" in:
      val game = """
    p
     K
    """ as White

      game.winner must beNone

    "not be playable" in:
      "with touching kings" in:
        val game = "kK BN" as Black
        game.playable(true) must beFalse
        game.playable(false) must beFalse

      "with other side in check" in:
        val game = "k Q K" as White
        game.playable(true) must beFalse
        game.playable(false) must beFalse

      "when previous move is a double pawn push and checker is not the pushed pawn or a sliding piece" in:
        val game1 = Fen
          .read(Standard, EpdFen("r1bqkbnr/1p1p1ppp/p7/2pPp3/4P3/5n2/PPP2PPP/RNBQKBNR w KQkq c6 0 4"))
          .get
        val game2 = Fen
          .read(Standard, EpdFen("r1bqkbnr/1p1p1ppp/p7/2pP4/4P3/8/PPP2pPP/RNBQKBNR w KQkq c6 0 4"))
          .get

        game1.playable(true) must beFalse
        game1.playable(false) must beFalse
        game2.playable(true) must beFalse
        game2.playable(false) must beFalse

      "when previous move is a double pawn push and the only checker is a rook but not discovered check" in:
        val game = Fen
          .read(Standard, EpdFen("1k6/5K1r/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HA c6 0 4"))
          .get
        game.playable(true) must beFalse
        game.playable(false) must beFalse

      "when previous move is a double pawn push and the only checker is a bishop but not discovered check" in:
        val game = Fen
          .read(Standard, EpdFen("2b4r/kr5p/p7/2pP2b1/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4"))
          .get
        game.playable(true) must beFalse
        game.playable(false) must beFalse

      "when multiple checkers are aligned with the king" in:
        val game = Fen
          .read(Standard, EpdFen("1nbqk3/1p1prppp/p1P5/8/4K3/8/PPP1rPPP/RNBQ1BNR b HA - 0 4"))
          .get
        game.playable(true) must beFalse
        game.playable(false) must beFalse

      "be playable" in:
        "when previous move is a double pawn push and the only checker is the pushed pawn" in:
          val game = Fen
            .read(Standard, EpdFen("r1bqkbnr/1p1p1ppp/p7/2pP4/3KP3/8/PPP3PP/RNBQ1BNR w HAkq c6 0 4"))
            .get
          game.playable(true) must beTrue
          game.playable(false) must beTrue

        "when two checkers are not on the same rank, file or diagonal" in:
          val game = Fen
            .read(Standard, EpdFen("rnbqk2r/1p1p1ppp/p1P5/3np1b1/4P3/4K3/PPP2PPP/RNBQ1BNR b HAkq - 0 4"))
            .get
          game.playable(true) must beTrue
          game.playable(false) must beTrue

        "when previous move is a double pawn push and the only checker is a discovered rook check" in:
          val game = Fen
            .read(Standard, EpdFen("1kb2b1r/1r3K1p/p7/2pP4/4P3/8/PPP3PP/RNBQ1BNR w HAk c6 0 4"))
            .get
          game.playable(true) must beTrue
          game.playable(false) must beTrue

        "when previous move is a double pawn push and the only checker is a discovered bishop check" in:
          val game = Fen
            .read(Standard, EpdFen("1bb4r/kr5p/p7/2pP4/4PK2/8/PPP3PP/RNBQ1BNR w HAh c6 0 4"))
            .get
          game.playable(true) must beTrue
          game.playable(false) must beTrue
