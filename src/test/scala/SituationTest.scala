package chess

class SituationTest extends ChessTest {

  "a game" should {
    "detect check" should {
      "by rook" in {
        ("""
K  r
""" as Red).check must beTrue
      }
      "by knight" in {
        ("""
  n
K
""" as Red).check must beTrue
      }
      "by bishop" in {
        ("""
  b

   
     K
""" as Red).check must beTrue
      }
      "by pawn" in {
        ("""
    p
     K
""" as Red).check must beTrue
      }
      "not" in {
        ("""
   n
K
""" as Red).check must beFalse
      }
    }
    "detect check mate" in {
      "by rook" in {
        ("""
PP
K  r
""" as Red).checkMate must beTrue
      }
      "by knight" in {
        ("""
PPn
KR
""" as Red).checkMate must beTrue
      }
      "not" in {
        ("""
  n
K
""" as Red).checkMate must beFalse
      }
    }
    "stale mate" in {
      "stuck in a corner" in {
        ("""
prr
K
""" as Red).staleMate must beTrue
      }
      "not" in {
        ("""
  b
K
""" as Red).staleMate must beFalse
      }
    }

    "Give the correct winner for a game" in {
      val game =
        """
PP
K  r
""" as Red

      game.checkMate must beTrue
      game.winner must beSome.like { case color =>
        color == Black
      }
    }

    "Not give a winner if the game is still in progress" in {
      val game = """
    p
     K
    """ as Red

      game.winner must beNone

    }

    "not be playable" in {
      "with touching kings" in {
        val game = "kK BN" as Black
        game.playable(true) must beFalse
        game.playable(false) must beFalse
      }

      "with other side in check" in {
        val game = "k Q K" as Red
        game.playable(true) must beFalse
        game.playable(false) must beFalse
      }
    }

  }
}
