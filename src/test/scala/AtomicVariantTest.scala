package chess

import chess.Variant.{Antichess, AtomicChess}

class AtomicVariantTest extends ChessTest {

  "Atomic chess" should {

    "Must explode surrounding non pawn pieces on capture" in {
      val fenPosition = "rnbqkbnr/1ppppp1p/p5p1/8/8/1P6/PBPPPPPP/RN1QKBNR w KQkq -"
      val maybeGame = fenToGame(fenPosition, AtomicChess)
      val explodedSquares = List(Pos.H8, Pos.G8)
      val intactPawns = List(Pos.F7, Pos.G6, Pos.H7)

      val explosionGame = maybeGame flatMap (_.playMoves((Pos.B2, Pos.H8)))

      explosionGame must beSuccess.like {
        case game =>
          explodedSquares.forall(pos => game.situation.board(pos).isEmpty) must beTrue
          intactPawns.forall(pos => game.situation.board(pos).isDefined) must beTrue
      }
    }

    "Must explode all surrounding non pawn pieces on capture (contrived situation)" in {
      val fenPosition = "k7/3bbn2/3rqn2/3qr3/8/7B/8/1K6 w - -"
      val maybeGame = fenToGame(fenPosition, AtomicChess)
      val explodedSquares = List(Pos.D5, Pos.E5, Pos.D6, Pos.E6, Pos.F6, Pos.D7, Pos.E7, Pos.F7)

      val explosionGame = maybeGame flatMap (_.playMoves((Pos.H3, Pos.E6)))

      explosionGame must beSuccess.like {
        case game =>
          explodedSquares.forall(pos => game.situation.board(pos).isEmpty) must beTrue
      }
    }

    "Must explode all surrounding non pawn pieces on capture (contrived situation with bottom right position)" in {
      val fenPosition = "k7/3bbn2/3rqn2/4rq2/8/1B6/8/K7 w - -"
      val maybeGame = fenToGame(fenPosition, AtomicChess)
      val explodedSquares = List(Pos.F5, Pos.E5, Pos.D6, Pos.E6, Pos.F6, Pos.D7, Pos.E7, Pos.F7)

      val explosionGame = maybeGame flatMap (_.playMoves((Pos.B3, Pos.E6)))

      explosionGame must beSuccess.like {
        case game =>
          explodedSquares.forall(pos => game.situation.board(pos).isEmpty) must beTrue
      }
    }

    "Not allow a king to capture a piece" in {
      val fenPosition = "8/8/8/1k6/8/8/8/1Kr5 w - -"
      val maybeGame = fenToGame(fenPosition, AtomicChess)

      val errorGame = maybeGame flatMap (_.playMoves((Pos.B1, Pos.C1)))

      errorGame must beFailure.like {
        case failMsg => failMsg mustEqual scalaz.NonEmptyList("Piece on b1 cannot move to c1")
      }
    }

    "The game must end with the correct winner when a king explodes in the perimeter of a captured piece" in {
      val fenPosition = "rnb1kbnr/ppp1pppp/8/3q4/8/7P/PPPP1PP1/RNBQKBNR b KQkq -"
      val maybeGame = fenToGame(fenPosition, AtomicChess)

      val gameWin = maybeGame flatMap (_.playMoves((Pos.D5, Pos.D2)))

      gameWin must beSuccess.like {
        case winningGame =>
          winningGame.situation.end must beTrue
          winningGame.situation.variantEnd must beTrue
          winningGame.situation.winner must beSome.like {
            case winner => winner == Black
          }
      }
    }

    "The game must end by a traditional checkmate (atomic mate)" in {
      val fenPosition = "1k6/8/8/8/8/8/PP5r/K7 b - -"
      val maybeGame = fenToGame(fenPosition, AtomicChess)

      val gameWin = maybeGame flatMap (_.playMoves((Pos.H2, Pos.H1)))

      gameWin must beSuccess.like {
        case winningGame =>
          winningGame.situation.end must beTrue
          winningGame.situation.variantEnd must beFalse
          winningGame.situation.winner must beSome.like { case color => color == Black}
      }
    }

    "Must be a stalemate if a king could usually take a piece to get out of check, but can't because it would explode" in {
      val positionFen = "k7/8/1R6/8/8/8/8/5K2 w - -"
      val maybeGame = fenToGame(positionFen, AtomicChess)

      val gameWin = maybeGame flatMap (_.playMoves((Pos.B6, Pos.B7)))

      gameWin must beSuccess.like {
        case game =>
          game.situation.moves
          game.situation.end must beTrue
          game.situation.staleMate must beTrue
      }
    }

    "It is stalemate if there are only two kings and two opposite square coloured bishops remaining" in {
      val positionFen = "4K3/8/2b5/8/8/8/5B2/3k4 b - -"
      val game = fenToGame(positionFen, AtomicChess)

      game must beSuccess.like {
        case game =>
          game.situation.end must beTrue
          game.situation.variantDraw must beTrue
          game.situation.winner must beNone
          game.situation.status must beSome.like{
            case status => status == Status.Draw
          }
      }
    }

    "In atomic check, an opportunity at exploding the opponent's king takes priority over getting out of check" in {
      val positionFen = "k1K5/pp5R/8/8/3Q4/P7/1P6/2r5 w - -"
      val threatenedGame = fenToGame(positionFen, AtomicChess)

      threatenedGame must beSuccess.like {
        case game =>
          game.situation.check must beTrue
          game.situation.end must beFalse
          game.situation.winner must beNone
          game.situation.moves must haveKeys(Pos.D4, Pos.H7, Pos.C8)
          game.situation.moves.get(Pos.D4) must beSome.like {
            case mvs => mvs.forall(_.captures) must beTrue
          }

          // The king cannot capture a piece in the perimeter of the opponent king, exploding itself
          game.situation.moves.get(Pos.C8) must beSome.like {
            case mvs => mvs.forall(_.captures) must beFalse
          }

          // The rook cannot capture, as that would result in our own king exploding
          game.situation.moves.get(Pos.H7) must beSome.like {
            case mvs =>
              mvs.find(_.captures) must beNone
              // It can, however, defend the king
              mvs.find(_.dest == Pos.C7) must beSome
              mvs.size must beEqualTo(1)
          }
      }
    }

    "In atomic mate, an opportunity at exploding the opponent's king takes priority over getting out of mate" in {
      val positionFen = "k1r5/pp5R/8/8/3Q4/8/PP6/K7 b - -"
      val game = fenToGame(positionFen, AtomicChess)

      val mateThreatedGame = game flatMap (_.playMoves((Pos.C8, Pos.C1)))

      mateThreatedGame must beSuccess.like {
        case game =>
          game.situation.end must beFalse
          game.situation.winner must beNone
          game.situation.moves must haveKeys(Pos.D4, Pos.H7)
          game.situation.moves.values.forall(_.forall(_.captures)) must beTrue
      }
    }

    "In atomic chess a king may walk into a square that is in the perimeter of the opponent king since it can't capture" in {
      val positionFen = "3k4/8/3K4/8/8/8/7r/8 w - -"
      val game = fenToGame(positionFen, AtomicChess)

      val successGame = game flatMap (_.playMoves((Pos.D6, Pos.D7)))

      successGame must beSuccess.like {
        case game =>
          game.situation.board(Pos.D7) must beSome
          game.situation.check must beFalse
      }
    }

    "Draw on knight and king vs king" in {
      val position = "8/1n6/8/8/8/8/k7/2K1b2R w - -"
      val game = fenToGame(position, AtomicChess)

      val successGame = game flatMap (_.playMoves((Pos.H1, Pos.E1)))

      successGame must beSuccess.like {
        case game =>
          game.situation.end must beTrue
          game.situation.status must beSome.like{
            case status => status == Status.Draw
          }
      }
    }

    "Draw on bishop and king vs king" in {
      val position = "8/1b6/8/8/8/8/k7/2K1n2R w - -"
      val game = fenToGame(position, AtomicChess)

      val successGame = game flatMap (_.playMoves((Pos.H1, Pos.E1)))

      successGame must beSuccess.like {
        case game =>
          game.situation.end must beTrue
          game.situation.status must beSome.like{
            case status => status == Status.Draw
          }
      }
    }

    "Draw on a rook and king vs king" in {
      val position = "8/8/8/8/8/8/N4r2/5k1K b - -"
      val game = fenToGame(position, AtomicChess)
      val successGame = game flatMap (_.playMoves((Pos.F2, Pos.A2)))
      successGame must beSuccess.like {
        case game =>
          game.situation.end must beTrue
          game.situation.status must beSome.like{
            case status => status == Status.Draw
          }
      }
    }

    "Draw on a king vs a king" in {
      val position = "6r1/8/8/1k6/8/8/2K5/6R1 w - -"
      val game = fenToGame(position, AtomicChess)
      val successGame = game flatMap (_.playMoves((Pos.G1, Pos.G8)))

      successGame must beSuccess.like {
        case game =>
          game.situation.end must beTrue
          game.situation.status must beSome.like{
            case status => status == Status.Draw
          }
      }
    }

  }

}
