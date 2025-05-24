package chess
package format.pgn
import scala.language.implicitConversions

class ReaderTest extends ChessTest:

  import Fixtures.*
  import Replay.*

  // "only raw moves" should:
  test("many games"):
    raws.foreach: c =>
      Replay
        .mainline(c)
        .assertRight:
          case Result(replay, None) => assertEquals(replay.moves.size, c.split(' ').length)
  test("example from prod 1"):
    assert(Replay.mainline(fromProd1).isRight)
  test("example from prod 2"):
    assert(Replay.mainline(fromProd2).isRight)
  test("rook promotion"):
    assert(Replay.mainline(promoteRook).isRight)
  test("castle check O-O-O+"):
    assert(Replay.mainline(castleCheck1).isRight)
  test("castle checkmate O-O#"):
    assert(Replay.mainline(castleCheck2).isRight)
  test("and delimiters"):
    Replay
      .mainline(withDelimiters)
      .assertRight:
        case Result(replay, None) => assertEquals(replay.moves.size, 33)
  test("and delimiters on new lines"):
    Replay
      .mainline(withDelimitersOnNewLines)
      .assertRight:
        case Result(replay, None) => assertEquals(replay.moves.size, 33)
  // "tags and moves" should:
  test("chess960"):
    assert(Replay.mainline(complete960).isRight)
  test("with empty lines"):
    assert(Replay.mainline("\n" + complete960 + "\n").isRight)
  test("example from wikipedia"):
    assert(Replay.mainline(fromWikipedia).isRight)
  test("with inline comments"):
    assert(Replay.mainline(inlineComments).isRight)
  test("example from chessgames.com"):
    assert(Replay.mainline(fromChessgames).isRight)
  test("example from chessgames.com with escape chars"):
    assert(Replay.mainline(fromChessgamesWithEscapeChar).isRight)
  test("immortal with NAG"):
    assert(Replay.mainline(withNag).isRight)
  test("example from TCEC"):
    assert(Replay.mainline(fromTcec).isRight)
  test("from https://chessprogramming.wikispaces.com/Kasparov+versus+Deep+Blue+1996"):
    assert(Replay.mainline(fromChessProgrammingWiki).isRight)
  test("comments and variations"):
    assert(Replay.mainline(commentsAndVariations).isRight)
  test("comments and variations by smartchess"):
    assert(Replay.mainline(bySmartChess).isRight)
  test("invalid variant"):
    Replay
      .mainline(invalidVariant)
      .assertRight:
        case Result(replay, None) =>
          assertEquals(replay.setup.position.variant, variant.Standard)
  test("promoting to a rook"):
    Replay
      .mainline(fromLichessBadPromotion)
      .assertRight:
        case Result(replay, None) =>
          replay.chronoMoves
            .lift(10)
            .assertSome: m =>
              assertEquals(m.fold(_.promotion, _ => None), Option(Rook))
  test("chessbase arrows"):
    assert(Replay.mainline(chessbaseArrows).isRight)
  test("atomic regression"):
    assert(Replay.mainline(atomicRegression).isRight)
  test("atomic promotion"):
    assert(Replay.mainline(atomicPromotion).isRight)
  test("lichobile export"):
    assert(Replay.mainline(lichobile).isRight)
  test("crazyhouse 1"):
    Replay
      .mainline(crazyhouse1)
      .assertRight:
        case Result(replay, None) =>
          replay.chronoMoves
            .lift(11)
            .assertSome: m =>
              assertEquals(m.toUci.uci, "P@c6")
  test("crazyhouse 2"):
    Replay
      .mainline(crazyhouse2)
      .assertRight:
        case Result(replay, None) => assertEquals(replay.chronoMoves.size, 111)
  test("crazyhouse without variant tag"):
    Replay
      .mainline(crazyhouseNoVariantTag)
      .assertRight:
        case Result(replay, _) =>
          assertEquals(replay.chronoMoves.size, 8)
  test("crazyhouse from chess.com"):
    assert(Replay.mainline(chessComCrazyhouse).isRight)

  // "from prod" in:
  test("from position close chess"):
    Replay
      .mainline(fromPosProdCloseChess)
      .assertRight:
        case Result(replay, None) =>
          assertEquals(replay.chronoMoves.size, 152)
  test("from position empty FEN"):
    Replay
      .mainline(fromPositionEmptyFen)
      .assertRight:
        case Result(replay, None) =>
          assertEquals(replay.chronoMoves.size, 164)
  test("preserves initial ply"):
    Replay
      .mainline(caissa)
      .assertRight:
        case Result(replay, None) =>
          assertEquals(replay.setup.startedAtPly, 43)

  test("partial from broadcast"):
    Replay
      .mainline(festivalFigueira)
      .assertRight:
        case Result(replay, _) =>
          assertEquals(replay.chronoMoves.size, 113)
  test("invisible char"):
    Replay
      .mainline(invisibleChar)
      .assertRight:
        case Result(replay, None) =>
          assertEquals(replay.chronoMoves.size, 19)

  test("exotic notation from clono.no"):
    Replay
      .mainline(clonoNoExoticNotation)
      .assertRight:
        case Result(replay, None) =>
          replay.chronoMoves
            .lift(42)
            .assertSome: m =>
              assertEquals(m.toUci.uci, "e7f8q")

  /*============== Error Messages ==============*/

  test("error message by white"):
    val pgn = PgnStr("1.e6")
    Replay
      .mainline(pgn)
      .assertRight:
        case Result(replay, Some(error)) =>
          assertEquals(error, ErrorStr("Cannot play e6 at move 1 by white"))

  test("error message by black"):
    val pgn = PgnStr("1.e4 e4")
    Replay
      .mainline(pgn)
      .assertRight:
        case Result(replay, Some(error)) =>
          assertEquals(error, ErrorStr("Cannot play e4 at move 1 by black"))

  test("more error message"):
    val pgn = PgnStr(
      "e3 Nc6 d4 Nf6 c3 e5 dxe5 Nxe5 Bb5 a6 Ba4 b5 Bb3 d5 e4 dxe4 f4 Qxd1+ Kxd1 Nd3 Be3 Ng4 Bd4 Ngf2+ Bxf2 Nxf2+ Ke1 Nxh1 Bd5 Ra7 Bc6+ Kd8 Bxe4 Bd6 g3 Re8 Nd2 f5 Ne2 fxe4 Kf1 e3 Kg2 exd2 Rxh1 Bb7+ Kf2 Bg3+ Kf3 d1=Q#"
    )
    Replay
      .mainline(pgn)
      .assertRight:
        case Result(replay, Some(error)) =>
          assertEquals(error, ErrorStr("Cannot play Bg3 at move 24 by black"))
