package chess
package format.pgn

import cats.syntax.option.*

import scala.language.implicitConversions

import Sans.*

class ParserTest extends ChessTest:

  import Fixtures.*

  given Conversion[SanStr, String]                                   = _.value
  given Conversion[String, SanStr]                                   = SanStr(_)
  extension (tree: Option[ParsedPgnTree]) def firstMove: PgnNodeData = tree.get.mainline.head.value
  extension (parsed: ParsedPgn) def metas                            = parsed.tree.get.value.metas

  import Parser.{ full as parse, move as parseMove }

  test("bom header should be ignored"):
    // "with tags" in:
    parse("\uFEFF[Event \"Some event\"]\n1. e4 e5").assertRight: parsed =>
      assertEquals(parsed.tags(_.Event), Some("Some event"))
      assertEquals(parsed.mainline.size, 2)

    // "without tags" in:
    parse("\uFEFF1. e4 e5 3. Nf3").assertRight: parsed =>
      assertEquals(parsed.mainline.size, 3)

    // "lowercase" in:
    parse("\ufeff1. e4 e5 3. Nf3").assertRight: parsed =>
      assertEquals(parsed.mainline.size, 3)

    // "in the middle of the string" in:
    parse("\ufeff1. e4 \ufeff e5 3. Nf3").assertRight: parsed =>
      assertEquals(parsed.mainline.size, 3)

  test("parse valid comment"):
    assert(Parser.pgnComment.parse("% comment").isRight)
  test("parse invalid comment"):
    assert(Parser.pgnComment.parse("  %comment").isLeft)

  test("promotion check as a queen"):
    parse("b8=Q ").assertRight: parsed =>
      parsed.mainline.headOption.assertSome: san =>
        assertEquals(san, Std(Square.B8, Pawn, promotion = Option(Queen)))

  test("promotion check as a rook"):
    parse("b8=R ").assertRight: parsed =>
      parsed.mainline.headOption.assertSome: san =>
        assertEquals(san.asInstanceOf[Std].promotion, Option(Rook))

  test("carriage return"):
    // "none" in:
    parse("1. e4 c6\n2. d4 d5").assertRight: parsed =>
      assertEquals(parsed.mainline.size, 4)
    // "one" in:
    parse("1. e4 c6\r\n2. d4 d5").assertRight: parsed =>
      assertEquals(parsed.mainline.size, 4)
    // "two" in:
    parse("1. e4 c6\r\r\n2. d4 d5").assertRight: parsed =>
      assertEquals(parsed.mainline.size, 4)
    // "between tags" in:
    parse("[White \"carriage\"]\r\n[Black \"return\"]\r\n\r\n1. a3 a6\r\n").assertRight: parsed =>
      assertEquals(parsed.tags(_.White), Some("carriage"))
      assertEquals(parsed.tags(_.Black), Some("return"))
      assertEquals(parsed.mainline.size, 2)

  test("result no tag but inline result"):
    parse(noTagButResult).assertRight: parsed =>
      assertEquals(parsed.tags("Result"), Option("1-0"))
  test("result in tags"):
    parse(whiteResignsInTags).assertRight: parsed =>
      assertEquals(parsed.tags("Result"), Option("0-1"))
  test("result in moves"):
    parse(whiteResignsInMoves).assertRight: parsed =>
      assertEquals(parsed.tags("Result"), Option("0-1"))
  test("result in tags and moves"):
    parse(whiteResignsInTagsAndMoves).assertRight: parsed =>
      assertEquals(parsed.tags("Result"), Option("0-1"))

  test("glyphs"):

    parseMove("b8=B ").assertRight: node =>
      assertEquals(node.value.san, Std(Square.B8, Pawn, promotion = Option(Bishop)))

    parseMove("1. e4").assertRight: node =>
      assertEquals(node.value.san, Std(Square.E4, Pawn))

    parseMove("e4").assertRight: node =>
      assertEquals(node.value.san, Std(Square.E4, Pawn))

    parseMove("e4!").assertRight: node =>
      assertEquals(node.value.san, Std(Square.E4, Pawn))
      assertEquals(node.value.metas.glyphs, Glyphs(Glyph.MoveAssessment.good.some, None, Nil))

    parseMove("Ne7g6+?!").assertRight: node =>
      assertEquals(node.value.san, Std(Square.G6, Knight, false, Some(File.E), Some(Rank.Seventh)))
      assertEquals(node.value.metas.glyphs, Glyphs(Glyph.MoveAssessment.dubious.some, None, Nil))

    parseMove("P@e4?!").assertRight: node =>
      assertEquals(node.value.san, Drop(Pawn, Square.E4))
      assertEquals(node.value.metas.glyphs, Glyphs(Glyph.MoveAssessment.dubious.some, None, Nil))

  test("nags"):
    assert(parse(withNag).isRight)

    parse("Ne7g6+! $13").assertRight: parsed =>
      assertEquals(parsed.metas.glyphs.move, Option(Glyph.MoveAssessment.good))
      assertEquals(parsed.metas.glyphs.position, Option(Glyph.PositionAssessment.unclear))

  test("non-nags"):
    assert(parse(withGlyphAnnotations).isRight)

    parse("Bxd3?? ∞").assertRight { parsed =>
      assertEquals(parsed.tree.firstMove.metas.glyphs.move, Option(Glyph.MoveAssessment.blunder))
      assertEquals(parsed.tree.firstMove.metas.glyphs.position, Option(Glyph.PositionAssessment.unclear))
    }

  test("comments"):
    parse("Ne7g6+! {such a neat comment}").assertRight: parsed =>
      assertEquals(parsed.tree.firstMove.metas.comments, List("such a neat comment"))

  test("variations"):
    parse("Ne7g6+! {such a neat comment} (e4 Ng6)").assertRight: parsed =>
      parsed.tree.get.variations.headOption.assertSome: variation =>
        assertEquals(variation.mainlineValues.size, 2)

  test("first move variation"):
    parse("1. e4 (1. d4)").assertRight: parsed =>
      parsed.tree.get.variations.headOption.assertSome: variation =>
        assertEquals(variation.mainlineValues.size, 1)

  raws.foreach: sans =>
    val size = sans.split(' ').length
    test(s"sans only size: $size"):
      parse(sans).assertRight: a =>
        assertEquals(a.mainline.size, size)

  (shortCastles ++ longCastles ++ annotatedCastles).foreach: sans =>
    val size = sans.split(' ').length
    test(s"sans only size: $size"):
      parse(sans).assertRight: a =>
        assertEquals(a.mainline.size, size)

  test("disambiguated"):
    parse(disambiguated).assertRight: a =>
      assertEquals(a.mainline.size, 3)

  List(fromProd1, fromProd2, castleCheck1, castleCheck2).foreach { sans =>
    val size = sans.split(' ').length
    test(s"sans only from prod size: $size"):
      parse(sans).assertRight: a =>
        assertEquals(a.mainline.size, size)
  }

  test("variations"):
    parse(variations).assertRight: a =>
      assertEquals(a.mainline.size, 20)

  test("inline tags"):
    parse(inlineTags).assertRight: a =>
      assert:
        a.tags.value.exists: tag =>
          tag.name == Tag.White && tag.value == "Blazquez, Denis"

  test("tag with nested quotes"):
    parse("""[Black "Schwarzenegger, Arnold \"The Terminator\""]""").assertRight: a =>
      assert:
        a.tags.value.exists: tag =>
          tag.name == Tag.Black && tag.value == """Schwarzenegger, Arnold "The Terminator""""

  test("time control tag with nested quote"):
    // yep, got that from https://pgnfiles.fide.com/budapest2024/women/round-1/women_r1_6.pgn
    parse("""[TimeControl "5400/40+30:1800+30\""]""").assertRight: a =>
      assertEquals(a.tags.value.headOption.map(_.value), """5400/40+30:1800+30""".some)

  test("tag with inner brackets"):
    parse("""[Black "[=0040.34h5a4]"]""").assertRight: a =>
      a.tags.value.exists: tag =>
        tag.name == Tag.Black && tag.value == "[=0040.34h5a4]"

  test("game from wikipedia"):
    parse(fromWikipedia).assertRight: a =>
      assertEquals(a.mainline.size, 85)

  test("game from crafty"):
    parse(fromCrafty).assertRight: a =>
      assertEquals(a.mainline.size, 68)

  test("inline comments"):
    parse(inlineComments).assertRight: a =>
      assertEquals(a.mainline.size, 85)

  test("block comment in variation root"):
    parse(rootCommentInVariation).assertRight: parsed =>
      assertEquals(parsed.tree.get.variations.head.value.variationComments, List("This other move:"))

  test("inline comment in variation root"):
    parse(rootCommentInVariation).assertRight: parsed =>
      assertEquals(parsed.tree.get.variations.tail.head.value.variationComments, List("Neither does :"))

  test("block comments in variation root"):
    parse(multipleRootCommentsInVariation).assertRight: parsed =>
      assertEquals(
        parsed.tree.get.variations.head.value.variationComments,
        List("This other move:", "looks pretty")
      )

  test("no block comment in variation root"):
    parse(variations).assertRight: parsed =>
      assertEquals(parsed.tree.get.variations.head.value.variationComments, Nil)

  test("multiple comments in variation root"):
    parse(multipleRootCommentsInVariation).assertRight: parsed =>
      assertEquals(
        parsed.tree.get.variations.tail.head.value.variationComments,
        List("Neither does :", "this or that", "or whatever")
      )

  test("comments and variations"):
    parse(commentsAndVariations).assertRight: parsed =>
      assertEquals(parsed.mainline.size, 103)

  test("comments and lines by smartchess"):
    parse(bySmartChess).assertRight: a =>
      assertEquals(a.mainline.size, 65)

  test("complete 960"):
    parse(complete960).assertRight: a =>
      assertEquals(a.mainline.size, 42)

  test("TCEC"):
    parse(fromTcec).assertRight: a =>
      assertEquals(a.mainline.size, 142)

  test("TCEC with engine output"):
    parse(fromTcecWithEngineOutput).assertRight: a =>
      assertEquals(a.mainline.size, 165)

  test("chesskids iphone"):
    parse(chesskids).assertRight: a =>
      assertEquals(a.mainline.size, 135)

  test("handwritten"):
    parse(handwritten).assertRight: a =>
      assertEquals(a.mainline.size, 139)

  test("chess by post"):
    parse(chessByPost).assertRight: a =>
      assertEquals(a.mainline.size, 100)

  test("Android device"):
    parse(android).assertRight: a =>
      assertEquals(a.mainline.size, 69)

  test("weird dashes"):
    parse(weirdDashes).assertRight: a =>
      assertEquals(a.mainline.size, 74)

  test("lichobile"):
    parse(lichobile).assertRight: a =>
      assertEquals(a.mainline.size, 68)

  test("overflow"):
    parse(overflow).assertRight: a =>
      assertEquals(a.mainline.size, 67)
  test("overflow 2"):
    parse(stackOverflow).assertRight: a =>
      assertEquals(a.mainline.size, 8)
  test("overflow 3"):
    parse(overflow3).assertRight: a =>
      assertEquals(a.mainline.size, 343)
  test("overflow 3: tags"):
    parse(overflow3).assertRight: a =>
      assertEquals(a.tags.value.size, 9)
  test("chessbase arrows"):
    parse(chessbaseArrows).assertRight: a =>
      assertEquals(a.initialPosition.comments, List("[%csl Gb4,Yd5,Rf6][%cal Ge2e4,Ye2d4,Re2g4]"))
  test("multiple initial comments with empty"):
    parse(multipleInitalCommentsWithEmpty).assertRight: a =>
      assertEquals(a.initialPosition.comments, List("this", "that"))
  test("chessbase weird"):
    parse(chessbaseWeird).assertRight: a =>
      assertEquals(a.mainline.size, 115)
  test("crazyhouse from prod"):
    parse(crazyhouseFromProd).assertRight: a =>
      assertEquals(a.mainline.size, 49)
  test("crazyhouse from chess.com"):
    parse(chessComCrazyhouse).assertRight: a =>
      assertEquals(a.mainline.size, 42)

  test("en passant e.p. notation"):
    parse(enpassantEP).assertRight: a =>
      assertEquals(a.mainline.size, 36)

    parse(enpassantEP2).assertRight: a =>
      assertEquals(a.mainline.size, 36)

  test("en passant ep notation"):
    parse(enpassantEP3).assertRight: a =>
      assertEquals(a.mainline.size, 5)

  test("year: full date"):
    parse(recentChessCom).assertRight: parsed =>
      assertEquals(parsed.tags.year, Option(2016))
  test("year: only year"):
    parse(explorerPartialDate).assertRight: parsed =>
      assertEquals(parsed.tags.year, Option(1978))

  test("weird variant names"):
    parse(stLouisFischerandom).assertRight: parsed =>
      assertEquals(parsed.tags.variant, Option(variant.Chess960))

  test("example from chessgames.com with weird comments"):
    assert(parse(chessgamesWeirdComments).isRight)

  test("exotic notation from clono.no"):
    assert(parse(clonoNoExoticNotation).isRight)

  test("example with tags & comments without moves 1"):
    assert(parse(tagsCommentsWithoutMoves1).isRight)

  test("example with tags & comments without moves 2"):
    assert(parse(tagsCommentsWithoutMoves2).isRight)

  test("empty spaces in tags value should be removed"):
    parse(completeTagsWithSpaces).assertRight: a =>
      assertEquals(a.tags(_.Variant), Some("Standard"))
      assertEquals(a.tags(_.Event), Some("Мат в 1 ход [1-63] - Дорофеева: Мат в 1 ход - 43"))

  test("game with comments"):
    parse(gameWithComments).assertRight: a =>
      assertEquals(a.mainline.size, 106)

  test("none break space"):
    val nbsp = "1.  e4 e5"
    parse(nbsp).assertRight: a =>
      assertEquals(a.mainline.size, 2)

  test("unicode result"):
    parse(unicodeResultDraw)
      .assertRight: a =>
        assertEquals(a.tags(_.Result), Some("½-½"))
        assertEquals(a.tags.outcome, Some(Outcome(None)))
    parse(unicodeResultWeirdDashDraw)
      .assertRight: a =>
        assertEquals(a.tags(_.Result), Some("1/2–1/2"))
        assertEquals(a.tags.outcome, Some(Outcome(None)))
    parse(unicodeResultWeirdDashWin)
      .assertRight: a =>
        assertEquals(a.tags(_.Result), Some("1–0"))
        assertEquals(a.tags.outcome, Some(Outcome(Some(White))))

  test("absent result"):
    parse(blackAbsentResult)
      .assertRight: a =>
        assertEquals(a.tags(_.Result), Some("+--"))
        assertEquals(a.tags.outcome, Some(Outcome(Some(White))))
