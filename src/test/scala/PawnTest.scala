package chess

import Pos._
import format.Uci

class PawnTest extends ChessTest {

  "a red pawn" should {

    "move towards rank by 1 square" in {
      makeBoard(
        A4 -> Red.pawn
      ) destsFrom A4 must bePoss(A5)
    }

    "not move to positions that are occupied by the same color" in {
      makeBoard(
        A4 -> Red.pawn,
        A5 -> Red.pawn
      ) destsFrom A4 must bePoss()
    }

    "capture in diagonal" in {
      makeBoard(
        D4 -> Red.pawn,
        C5 -> Black.pawn,
        E5 -> Black.bishop
      ) destsFrom D4 must bePoss(C5, D5, E5)
    }

    "require a capture to move in diagonal" in {
      makeBoard(
        A4 -> Red.pawn,
        C5 -> Red.pawn
      ) destsFrom A4 must bePoss(A5)
    }

    "move towards rank by 2 squares" in {
      "if the path is free" in {
        makeBoard(
          A2 -> Red.pawn
        ) destsFrom A2 must bePoss(A3, A4)
      }
      "if the path is occupied by a friend" in {
        "close" in {
          makeBoard(
            A2 -> Red.pawn,
            A3 -> Red.rook
          ) destsFrom A2 must bePoss()
        }
        "far" in {
          makeBoard(
            A2 -> Red.pawn,
            A4 -> Red.rook
          ) destsFrom A2 must bePoss(A3)
        }
      }
      "if the path is occupied by a enemy" in {
        "close" in {
          makeBoard(
            A2 -> Red.pawn,
            A3 -> Black.rook
          ) destsFrom A2 must bePoss()
        }
        "far" in {
          makeBoard(
            A2 -> Red.pawn,
            A4 -> Black.rook
          ) destsFrom A2 must bePoss(A3)
        }
      }
    }
    "capture en passant" in {
      "with proper position" in {
        val board = makeBoard(
          D5 -> Red.pawn,
          C5 -> Black.pawn,
          E5 -> Black.pawn
        )
        "without history" in {
          board destsFrom D5 must bePoss(D6)
        }
        "with irrelevant history" in {
          board withHistory History(
            lastMove = Option(Uci.Move(A2, A4))
          ) destsFrom D5 must bePoss(D6)
        }
        "with relevant history on the left" in {
          board withHistory History(
            lastMove = Option(Uci.Move(C7, C5))
          ) destsFrom D5 must bePoss(D6, C6)
        }
        "with relevant history on the right" in {
          board withHistory History(
            lastMove = Option(Uci.Move(E7, E5))
          ) destsFrom D5 must bePoss(D6, E6)
        }
      }
      "enemy not-a-pawn" in {
        makeBoard(
          D5 -> Red.pawn,
          E5 -> Black.rook
        ) withHistory History(
          lastMove = Option(Uci.Move(E7, E5))
        ) destsFrom D5 must bePoss(D6)
      }
      "friend pawn (?!)" in {
        makeBoard(
          D5 -> Red.pawn,
          E5 -> Red.pawn
        ) withHistory History(
          lastMove = Option(Uci.Move(E7, E5))
        ) destsFrom D5 must bePoss(D6)
      }
    }
  }

  "a black pawn" should {

    "move towards rank by 1 square" in {
      makeBoard(
        A4 -> Black.pawn
      ) destsFrom A4 must bePoss(A3)
    }

    "not move to positions that are occupied by the same color" in {
      makeBoard(
        A4 -> Black.pawn,
        A3 -> Black.pawn
      ) destsFrom A4 must bePoss()
    }

    "capture in diagonal" in {
      makeBoard(
        D4 -> Black.pawn,
        C3 -> Red.pawn,
        E3 -> Red.bishop
      ) destsFrom D4 must bePoss(C3, D3, E3)
    }

    "require a capture to move in diagonal" in {
      makeBoard(
        A4 -> Black.pawn,
        C3 -> Black.pawn
      ) destsFrom A4 must bePoss(A3)
    }

    "move towards rank by 2 squares" in {
      "if the path is free" in {
        makeBoard(
          A7 -> Black.pawn
        ) destsFrom A7 must bePoss(A6, A5)
      }
      "if the path is occupied by a friend" in {
        "close" in {
          makeBoard(
            A7 -> Black.pawn,
            A6 -> Black.rook
          ) destsFrom A7 must bePoss()
        }
        "far" in {
          makeBoard(
            A7 -> Black.pawn,
            A5 -> Black.rook
          ) destsFrom A7 must bePoss(A6)
        }
      }
      "if the path is occupied by a enemy" in {
        "close" in {
          makeBoard(
            A7 -> Black.pawn,
            A6 -> Red.rook
          ) destsFrom A7 must bePoss()
        }
        "far" in {
          makeBoard(
            A7 -> Black.pawn,
            A5 -> Red.rook
          ) destsFrom A7 must bePoss(A6)
        }
      }
    }
    "capture en passant" in {
      "with proper position" in {
        val board = makeBoard(
          D4 -> Black.pawn,
          C4 -> Red.pawn,
          E4 -> Red.pawn
        )
        "without history" in {
          board destsFrom D4 must bePoss(D3)
        }
        "with relevant history on the left" in {
          board withHistory History(
            lastMove = Option(Uci.Move(C2, C4))
          ) destsFrom D4 must bePoss(D3, C3)
        }
      }
      "enemy not-a-pawn" in {
        makeBoard(
          D4 -> Black.pawn,
          E4 -> Red.rook
        ) withHistory History(
          lastMove = Option(Uci.Move(E2, E4))
        ) destsFrom D4 must bePoss(D3)
      }
    }
  }
}
