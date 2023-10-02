package chess
package format.pgn

import scala.language.implicitConversions
import MoveOrDrop.*

class ReaderTest extends ChessTest:

  import Fixtures.*
  import Reader.Result.*

  "only raw moves" should:
    "many games" in:
      forall(raws) { (c: String) =>
        Reader.full(c) must beRight.like { case Complete(replay) =>
          replay.moves must have size c.split(' ').length
        }
      }
    "example from prod 1" in:
      Reader.full(fromProd1) must beRight
    "example from prod 2" in:
      Reader.full(fromProd2) must beRight
    "rook promotion" in:
      Reader.full(promoteRook) must beRight
    "castle check O-O-O+" in:
      Reader.full(castleCheck1) must beRight
    "castle checkmate O-O#" in:
      Reader.full(castleCheck2) must beRight
    "and delimiters" in:
      Reader.full(withDelimiters) must beRight.like { case Complete(replay) =>
        replay.moves must have size 33
      }
    "and delimiters on new lines" in:
      Reader.full(withDelimitersOnNewLines) must beRight.like { case Complete(replay) =>
        replay.moves must have size 33
      }
  "tags and moves" should:
    "chess960" in:
      Reader.full(complete960) must beRight
    "with empty lines" in:
      Reader.full("\n" + complete960 + "\n") must beRight
    "example from wikipedia" in:
      Reader.full(fromWikipedia) must beRight
    "with inline comments" in:
      Reader.full(inlineComments) must beRight
    "example from chessgames.com" in:
      Reader.full(fromChessgames) must beRight
    "example from chessgames.com with escape chars" in:
      Reader.full(fromChessgamesWithEscapeChar) must beRight
    "immortal with NAG" in:
      Reader.full(withNag) must beRight
    "example from TCEC" in:
      Reader.full(fromTcec) must beRight
    "from https://chessprogramming.wikispaces.com/Kasparov+versus+Deep+Blue+1996" in:
      Reader.full(fromChessProgrammingWiki) must beRight
    "comments and variations" in:
      Reader.full(commentsAndVariations) must beRight
    "comments and variations by smartchess" in:
      Reader.full(bySmartChess) must beRight
    "invalid variant" in:
      Reader.full(invalidVariant) must beRight.like { case Complete(replay) =>
        replay.setup.board.variant must_== variant.Standard
      }
    "promoting to a rook" in:
      Reader.full(fromLichessBadPromotion) must beRight.like { case Complete(replay) =>
        replay.chronoMoves lift 10 must beSome:
          (_: MoveOrDrop).fold(_.promotion, _ => None) must_== Some(Rook)
      }
    "chessbase arrows" in:
      Reader.full(chessbaseArrows) must beRight
    "atomic regression" in:
      Reader.full(atomicRegression) must beRight
    "atomic promotion" in:
      Reader.full(atomicPromotion) must beRight
    "lichobile export" in:
      Reader.full(lichobile) must beRight
    "crazyhouse 1" in:
      Reader.full(crazyhouse1) must beRight.like { case Complete(replay) =>
        replay.chronoMoves lift 11 must beSome:
          (_: MoveOrDrop).toUci.uci must_== "P@c6"
      }
    "crazyhouse 2" in:
      Reader.full(crazyhouse2) must beRight.like { case Complete(replay) =>
        replay.chronoMoves.size must_== 111
      }
    "crazyhouse without variant tag" in:
      Reader.full(crazyhouseNoVariantTag) must beRight.like { case Incomplete(replay, _) =>
        replay.chronoMoves.size must_== 8
      }
    "crazyhouse from chess.com" in:
      Reader.full(chessComCrazyhouse) must beRight
  "from prod" in:
    "from position close chess" in:
      Reader.full(fromPosProdCloseChess) must beRight.like { case Complete(replay) =>
        replay.chronoMoves.size must_== 152
      }
    "from position empty FEN" in:
      Reader.full(fromPositionEmptyFen) must beRight.like { case Complete(replay) =>
        replay.chronoMoves.size must_== 164
      }
    "preserves initial ply" in:
      Reader.full(caissa) must beRight.like { case Complete(replay) =>
        replay.setup.startedAtPly must_== 43
        replay.state.startedAtPly must_== 43
      }
  "partial from broadcast" in:
    Reader.full(festivalFigueira) must beRight.like { case Incomplete(replay, _) =>
      replay.chronoMoves.size must_== 113
    }
  "invisible char" in:
    Reader.full(invisibleChar) must beRight.like { case Complete(replay) =>
      replay.chronoMoves.size must_== 19
    }
  "exotic notation from clono.no" in:
    Reader.full(clonoNoExoticNotation) must beRight.like { case Complete(replay) =>
      replay.chronoMoves lift 42 must beSome { (m: MoveOrDrop) =>
        m.toUci.uci must_== "e7f8q"
      }
    }
