package chess
package format.pgn

class ReaderTest extends ChessTest:

  import Fixtures.*
  import Reader.Result.*

  "only raw moves" should {
    "many games" in {
      forall(raws) { (c: String) =>
        Reader.full(c) must beValid.like { case Complete(replay) =>
          replay.moves must have size c.split(' ').length
        }
      }
    }
    "example from prod 1" in {
      Reader.full(fromProd1) must beValid
    }
    "example from prod 2" in {
      Reader.full(fromProd2) must beValid
    }
    "rook promotion" in {
      Reader.full(promoteRook) must beValid
    }
    "castle check O-O-O+" in {
      Reader.full(castleCheck1) must beValid
    }
    "castle checkmate O-O#" in {
      Reader.full(castleCheck2) must beValid
    }
    "and delimiters" in {
      Reader.full(withDelimiters) must beValid.like { case Complete(replay) =>
        replay.moves must have size 33
      }
    }
    "and delimiters on new lines" in {
      Reader.full(withDelimitersOnNewLines) must beValid.like { case Complete(replay) =>
        replay.moves must have size 33
      }
    }
  }
  "tags and moves" should {
    "chess960" in {
      Reader.full(complete960) must beValid
    }
    "with empty lines" in {
      Reader.full("\n" + complete960 + "\n") must beValid
    }
    "example from wikipedia" in {
      Reader.full(fromWikipedia) must beValid
    }
    "with inline comments" in {
      Reader.full(inlineComments) must beValid
    }
    "example from chessgames.com" in {
      Reader.full(fromChessgames) must beValid
    }
    "example from chessgames.com with escape chars" in {
      Reader.full(fromChessgamesWithEscapeChar) must beValid
    }
    "immortal with NAG" in {
      Reader.full(withNag) must beValid
    }
    "example from TCEC" in {
      Reader.full(fromTcec) must beValid
    }
    "from https://chessprogramming.wikispaces.com/Kasparov+versus+Deep+Blue+1996" in {
      Reader.full(fromChessProgrammingWiki) must beValid
    }
    "comments and variations" in {
      Reader.full(commentsAndVariations) must beValid
    }
    "comments and variations by smartchess" in {
      Reader.full(bySmartChess) must beValid
    }
    "invalid variant" in {
      Reader.full(invalidVariant) must beValid.like { case Complete(replay) =>
        replay.setup.board.variant must_== variant.Standard
      }
    }
    "promoting to a rook" in {
      Reader.full(fromLichessBadPromotion) must beValid.like { case Complete(replay) =>
        replay.chronoMoves lift 10 must beSome {
          (_: MoveOrDrop).fold(_.promotion, _ => None) must_== Option(Rook)
        }
      }
    }
    "chessbase arrows" in {
      Reader.full(chessbaseArrows) must beValid
    }
    "atomic regression" in {
      Reader.full(atomicRegression) must beValid
    }
    "atomic promotion" in {
      Reader.full(atomicPromotion) must beValid
    }
    "lichobile export" in {
      Reader.full(lichobile) must beValid
    }
    "crazyhouse 1" in {
      Reader.full(crazyhouse1) must beValid.like { case Complete(replay) =>
        replay.chronoMoves lift 11 must beSome {
          (_: MoveOrDrop).fold(_.toUci.uci, _.toUci.uci) must_== "P@c6"
        }
      }
    }
    "crazyhouse 2" in {
      Reader.full(crazyhouse2) must beValid.like { case Complete(replay) =>
        replay.chronoMoves.size must_== 111
      }
    }
    "crazyhouse without variant tag" in {
      Reader.full(crazyhouseNoVariantTag) must beValid.like { case Incomplete(replay, _) =>
        replay.chronoMoves.size must_== 8
      }
    }
    "crazyhouse from chess.com" in {
      Reader.full(chessComCrazyhouse) must beValid
    }
  }
  "from prod" in {
    "from position close chess" in {
      Reader.full(fromPosProdCloseChess) must beValid.like { case Complete(replay) =>
        replay.chronoMoves.size must_== 152
      }
    }
    "from position empty FEN" in {
      Reader.full(fromPositionEmptyFen) must beValid.like { case Complete(replay) =>
        replay.chronoMoves.size must_== 164
      }
    }
    "preserves initial ply" in {
      Reader.full(caissa) must beValid.like { case Complete(replay) =>
        replay.setup.startedAtTurn must_== 43
        replay.state.startedAtTurn must_== 43
      }
    }
  }
  "partial from broadcast" in {
    Reader.full(festivalFigueira) must beValid.like { case Incomplete(replay, _) =>
      replay.chronoMoves.size must_== 113
    }
  }
  "invisible char" in {
    Reader.full(invisibleChar) must beValid.like { case Complete(replay) =>
      replay.chronoMoves.size must_== 19
    }
  }
  "exotic notation from clono.no" in {
    Reader.full(clonoNoExoticNotation) must beValid.like { case Complete(replay) =>
      replay.chronoMoves lift 42 must beSome {
        (_: MoveOrDrop) must beLeft { (_: chess.Move).toUci.uci must_== "e7f8q" }
      }
    }
  }
