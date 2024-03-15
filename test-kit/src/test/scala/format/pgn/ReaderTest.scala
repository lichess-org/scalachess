package chess
package format.pgn

import scala.language.implicitConversions
import MoveOrDrop.*

class ReaderTest extends ChessTest:

  import Fixtures.*
  import Reader.Result.*

  // "only raw moves" should:
  test("many games"):
    raws.foreach: c =>
      Reader
        .full(c)
        .assertRight:
          case Complete(replay) => assertEquals(replay.moves.size, c.split(' ').length)
  test("example from prod 1"):
    assert(Reader.full(fromProd1).isRight)
  test("example from prod 2"):
    assert(Reader.full(fromProd2).isRight)
  test("rook promotion"):
    assert(Reader.full(promoteRook).isRight)
  test("castle check O-O-O+"):
    assert(Reader.full(castleCheck1).isRight)
  test("castle checkmate O-O#"):
    assert(Reader.full(castleCheck2).isRight)
  test("and delimiters"):
    Reader
      .full(withDelimiters)
      .assertRight:
        case Complete(replay) => assertEquals(replay.moves.size, 33)
  test("and delimiters on new lines"):
    Reader
      .full(withDelimitersOnNewLines)
      .assertRight:
        case Complete(replay) => assertEquals(replay.moves.size, 33)
  // "tags and moves" should:
  test("chess960"):
    assert(Reader.full(complete960).isRight)
  test("with empty lines"):
    assert(Reader.full("\n" + complete960 + "\n").isRight)
  test("example from wikipedia"):
    assert(Reader.full(fromWikipedia).isRight)
  test("with inline comments"):
    assert(Reader.full(inlineComments).isRight)
  test("example from chessgames.com"):
    assert(Reader.full(fromChessgames).isRight)
  test("example from chessgames.com with escape chars"):
    assert(Reader.full(fromChessgamesWithEscapeChar).isRight)
  test("immortal with NAG"):
    assert(Reader.full(withNag).isRight)
  test("example from TCEC"):
    assert(Reader.full(fromTcec).isRight)
  test("from https://chessprogramming.wikispaces.com/Kasparov+versus+Deep+Blue+1996"):
    assert(Reader.full(fromChessProgrammingWiki).isRight)
  test("comments and variations"):
    assert(Reader.full(commentsAndVariations).isRight)
  test("comments and variations by smartchess"):
    assert(Reader.full(bySmartChess).isRight)
  test("invalid variant"):
    Reader
      .full(invalidVariant)
      .assertRight:
        case Complete(replay) =>
          assertEquals(replay.setup.board.variant, variant.Standard)
  test("promoting to a rook"):
    Reader
      .full(fromLichessBadPromotion)
      .assertRight:
        case Complete(replay) =>
          replay.chronoMoves
            .lift(10)
            .assertSome: m =>
              assertEquals(m.fold(_.promotion, _ => None), Option(Rook))
  test("chessbase arrows"):
    assert(Reader.full(chessbaseArrows).isRight)
  test("atomic regression"):
    assert(Reader.full(atomicRegression).isRight)
  test("atomic promotion"):
    assert(Reader.full(atomicPromotion).isRight)
  test("lichobile export"):
    assert(Reader.full(lichobile).isRight)
  test("crazyhouse 1"):
    Reader
      .full(crazyhouse1)
      .assertRight:
        case Complete(replay) =>
          replay.chronoMoves
            .lift(11)
            .assertSome: m =>
              assertEquals(m.toUci.uci, "P@c6")
  test("crazyhouse 2"):
    Reader
      .full(crazyhouse2)
      .assertRight:
        case Complete(replay) => assertEquals(replay.chronoMoves.size, 111)
  test("crazyhouse without variant tag"):
    Reader
      .full(crazyhouseNoVariantTag)
      .assertRight:
        case Incomplete(replay, _) =>
          assertEquals(replay.chronoMoves.size, 8)
  test("crazyhouse from chess.com"):
    assert(Reader.full(chessComCrazyhouse).isRight)

  // "from prod" in:
  test("from position close chess"):
    Reader
      .full(fromPosProdCloseChess)
      .assertRight:
        case Complete(replay) =>
          assertEquals(replay.chronoMoves.size, 152)
  test("from position empty FEN"):
    Reader
      .full(fromPositionEmptyFen)
      .assertRight:
        case Complete(replay) =>
          assertEquals(replay.chronoMoves.size, 164)
  test("preserves initial ply"):
    Reader
      .full(caissa)
      .assertRight:
        case Complete(replay) =>
          assertEquals(replay.setup.startedAtPly, 43)

  test("partial from broadcast"):
    Reader
      .full(festivalFigueira)
      .assertRight:
        case Incomplete(replay, _) =>
          assertEquals(replay.chronoMoves.size, 113)
  test("invisible char"):
    Reader
      .full(invisibleChar)
      .assertRight:
        case Complete(replay) =>
          assertEquals(replay.chronoMoves.size, 19)
  test("exotic notation from clono.no"):
    Reader
      .full(clonoNoExoticNotation)
      .assertRight:
        case Complete(replay) =>
          replay.chronoMoves
            .lift(42)
            .assertSome: m =>
              assertEquals(m.toUci.uci, "e7f8q")
