package chess
package format.pgn

import cats.syntax.option.*
import scala.language.implicitConversions
import Sans.*

class ParserTest extends ChessTest:

  import Fixtures.*

  given Conversion[SanStr, String] = _.value
  given Conversion[String, SanStr] = SanStr(_)

  import Parser.{ full as parse, move as parseMove }

  extension (tree: Option[ParsedPgnTree]) def firstMove: PgnNodeData = tree.get.mainline.head.value

  extension (parsed: ParsedPgn) def metas = parsed.tree.get.value.metas

  "bom header" should:
    "be ignored" in:
      "with tags" in:
        parse("\uFEFF[Event \"Some event\"]\n1. e4 e5") must beRight.like: parsed =>
          parsed.tags(_.Event) must_== Some("Some event")
          parsed.mainline.size must_== 2

      "without tags" in:
        parse("\uFEFF1. e4 e5 3. Nf3") must beRight.like: parsed =>
          parsed.mainline.size must_== 3

      "lowercase" in:
        parse("\ufeff1. e4 e5 3. Nf3") must beRight.like: parsed =>
          parsed.mainline.size must_== 3

      "in the middle of the string" in:
        parse("\ufeff1. e4 \ufeff e5 3. Nf3") must beRight.like: parsed =>
          parsed.mainline.size must_== 3

  "pgnComment" should:
    "parse valid comment" in:
      Parser.pgnComment.parse("% comment") must beRight
    "parse invalid comment" in:
      Parser.pgnComment.parse("  %comment") must beLeft

  "promotion check" should:
    "as a queen" in:
      parse("b8=Q ") must beRight.like: parsed =>
        parsed.mainline.headOption must beSome { (san: San) =>
          san === Std(Square.B8, Pawn, promotion = Some(Queen))
        }

    "as a rook" in:
      parse("b8=R ") must beRight { (parsed: ParsedPgn) =>
        parsed.mainline.headOption must beSome { (san: San) =>
          san.asInstanceOf[Std].promotion must_== Some(Rook)
        }
      }

  "carriage return" in:
    "none" in:
      parse("1. e4 c6\n2. d4 d5") must beRight.like { parsed =>
        parsed.mainline.size must_== 4
      }
    "one" in:
      parse("1. e4 c6\r\n2. d4 d5") must beRight.like { parsed =>
        parsed.mainline.size must_== 4
      }
    "two" in:
      parse("1. e4 c6\r\r\n2. d4 d5") must beRight.like { parsed =>
        parsed.mainline.size must_== 4
      }
    "between tags" in:
      parse("[White \"carriage\"]\r\n[Black \"return\"]\r\n\r\n1. a3 a6\r\n") must beRight.like: parsed =>
        parsed.tags(_.White) must_== Some("carriage")
        parsed.tags(_.Black) must_== Some("return")
        parsed.mainline.size must_== 2

  "result" in:
    "no tag but inline result" in:
      parse(noTagButResult) must beRight.like { parsed =>
        parsed.tags("Result") must_== Option("1-0")
      }
    "in tags" in:
      parse(whiteResignsInTags) must beRight.like { parsed =>
        parsed.tags("Result") must_== Option("0-1")
      }
    "in moves" in:
      parse(whiteResignsInMoves) must beRight.like { parsed =>
        parsed.tags("Result") must_== Option("0-1")
      }
    "in tags and moves" in:
      parse(whiteResignsInTagsAndMoves) must beRight.like { parsed =>
        parsed.tags("Result") must_== Option("0-1")
      }

  "glyphs" in:

    parseMove("b8=B ") must beRight.like: node =>
      node.value.san === Std(Square.B8, Pawn, promotion = Some(Bishop))

    parseMove("1. e4") must beRight.like: node =>
      node.value.san must_== Std(Square.E4, Pawn)

    parseMove("e4") must beRight.like: node =>
      node.value.san must_== Std(Square.E4, Pawn)

    parseMove("e4!") must beRight.like: node =>
      node.value.san === Std(Square.E4, Pawn)
      node.value.metas.glyphs === Glyphs(Glyph.MoveAssessment.good.some, None, Nil)

    parseMove("Ne7g6+?!") must beRight.like: node =>
      node.value.san === Std(Square.G6, Knight, false, Some(File.E), Some(Rank.Seventh))
      node.value.metas.glyphs === Glyphs(Glyph.MoveAssessment.dubious.some, None, Nil)

    parseMove("P@e4?!") must beRight.like: node =>
      node.value.san === Drop(Pawn, Square.E4)
      node.value.metas.glyphs === Glyphs(Glyph.MoveAssessment.dubious.some, None, Nil)

  "nags" in:
    parse(withNag) must beRight

    parse("Ne7g6+! $13") must beRight.like: parsed =>
      parsed.metas.glyphs.move must_== Some(Glyph.MoveAssessment.good)
      parsed.metas.glyphs.position must_== Some(Glyph.PositionAssessment.unclear)

  "non-nags" in:
    parse(withGlyphAnnotations) must beRight

    parse("Bxd3?? ∞") must beRight.like { parsed =>
      parsed.tree.firstMove.metas.glyphs.move must_== Some(Glyph.MoveAssessment.blunder)
      parsed.tree.firstMove.metas.glyphs.position must_== Some(Glyph.PositionAssessment.unclear)
    }

  "comments" in:
    parse("Ne7g6+! {such a neat comment}") must beRight.like { parsed =>
      parsed.tree.firstMove.metas.comments must_== List("such a neat comment")
    }

  "variations" in:
    parse("Ne7g6+! {such a neat comment} (e4 Ng6)") must beRight.like: parsed =>
      parsed.tree.get.variations.headOption must beSome:
        (_: Variation[PgnNodeData]).mainlineValues must haveSize(2)

  "first move variation" in:
    parse("1. e4 (1. d4)") must beRight.like: parsed =>
      parsed.tree.get.variations.headOption must beSome:
        (_: Variation[PgnNodeData]).mainlineValues must haveSize(1)

  raws foreach { sans =>
    val size = sans.split(' ').length
    "sans only size: " + size in:
      parse(sans) must beRight.like { a =>
        a.mainline.size must_== size
      }
  }

  (shortCastles ++ longCastles ++ annotatedCastles) foreach { sans =>
    val size = sans.split(' ').length
    "sans only size: " + size in:
      parse(sans) must beRight.like: a =>
        a.mainline.size must_== size
  }

  "disambiguated" in:
    parse(disambiguated) must beRight.like { a =>
      a.mainline.size must_== 3
    }

  List(fromProd1, fromProd2, castleCheck1, castleCheck2) foreach { sans =>
    val size = sans.split(' ').length
    "sans only from prod size: " + size in:
      parse(sans) must beRight.like { a =>
        a.mainline.size must_== size
      }
  }

  "variations" in:
    parse(variations) must beRight.like { a =>
      a.mainline.size must_== 20
    }

  "inline tags" in:
    parse(inlineTags) must beRight.like { a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.White && tag.value == "Blazquez, Denis"
      }
    }

  "tag with nested quotes" in:
    parse("""[Black "Schwarzenegger, Arnold \"The Terminator\""]""") must beRight.like { a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Black && tag.value == """Schwarzenegger, Arnold "The Terminator""""
      }
    }

  "tag with inner brackets" in:
    parse("""[Black "[=0040.34h5a4]"]""") must beRight.like { a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Black && tag.value == "[=0040.34h5a4]"
      }
    }

  "game from wikipedia" in:
    parse(fromWikipedia) must beRight.like { a =>
      a.mainline.size must_== 85
    }

  "game from crafty" in:
    parse(fromCrafty) must beRight.like { a =>
      a.mainline.size must_== 68
    }

  "inline comments" in:
    parse(inlineComments) must beRight.like { a =>
      a.mainline.size must_== 85
    }

  "block comment in variation root" in:
    parse(rootCommentInVariation) must beRight.like: parsed =>
      parsed.tree.get.variations.head.value.variationComments must_==
        List("This other move:")

  "inline comment in variation root" in:
    parse(rootCommentInVariation) must beRight.like: parsed =>
      parsed.tree.get.variations.tail.head.value.variationComments must_==
        List("Neither does :")

  "block comments in variation root" in:
    parse(multipleRootCommentsInVariation) must beRight.like: parsed =>
      parsed.tree.get.variations.head.value.variationComments must_==
        List("This other move:", "looks pretty")

  "no block comment in variation root" in:
    parse(variations) must beRight.like: parsed =>
      parsed.tree.get.variations.head.value.variationComments must_== Nil

  "multiple comments in variation root" in:
    parse(multipleRootCommentsInVariation) must beRight.like: parsed =>
      parsed.tree.get.variations.tail.head.value.variationComments must_==
        List("Neither does :", "this or that", "or whatever")

  "comments and variations" in:
    parse(commentsAndVariations) must beRight.like: parsed =>
      parsed.mainline.size must_== 103

  "comments and lines by smartchess" in:
    parse(bySmartChess) must beRight.like { a =>
      a.mainline.size must_== 65
    }

  "complete 960" in:
    parse(complete960) must beRight.like { a =>
      a.mainline.size must_== 42
    }

  "TCEC" in:
    parse(fromTcec) must beRight.like { a =>
      a.mainline.size must_== 142
    }

  "TCEC with engine output" in:
    parse(fromTcecWithEngineOutput) must beRight.like { a =>
      a.mainline.size must_== 165
    }

  "chesskids iphone" in:
    parse(chesskids) must beRight.like { a =>
      a.mainline.size must_== 135
    }

  "handwritten" in:
    parse(handwritten) must beRight.like { a =>
      a.mainline.size must_== 139
    }

  "chess by post" in:
    parse(chessByPost) must beRight.like { a =>
      a.mainline.size must_== 100
    }

  "Android device" in:
    parse(android) must beRight.like { a =>
      a.mainline.size must_== 69
    }

  "weird dashes" in:
    parse(weirdDashes) must beRight.like { a =>
      a.mainline.size must_== 74
    }

  "lichobile" in:
    parse(lichobile) must beRight.like { a =>
      a.mainline.size must_== 68
    }

  "overflow" in:
    parse(overflow) must beRight.like { a =>
      a.mainline.size must_== 67
    }
  "overflow 2" in:
    parse(stackOverflow) must beRight.like { a =>
      a.mainline.size must_== 8
    }
  "overflow 3" in:
    parse(overflow3) must beRight.like { a =>
      a.mainline.size must_== 343
    }
  "overflow 3: tags" in:
    parse(overflow3) must beRight.like { a =>
      a.tags.value.size must_== 9
    }
  "chessbase arrows" in:
    parse(chessbaseArrows) must beRight.like { a =>
      a.initialPosition.comments must_== List(
        "[%csl Gb4,Yd5,Rf6][%cal Ge2e4,Ye2d4,Re2g4]"
      )
    }
  "multiple initial comments with empty" in:
    parse(multipleInitalCommentsWithEmpty) must beRight.like { a =>
      a.initialPosition must_== List("this", "that")
    }
  "chessbase weird" in:
    parse(chessbaseWeird) must beRight.like { a =>
      a.mainline.size must_== 115
    }
  "crazyhouse from prod" in:
    parse(crazyhouseFromProd) must beRight.like { a =>
      a.mainline.size must_== 49
    }
  "crazyhouse from chess.com" in:
    parse(chessComCrazyhouse) must beRight.like { a =>
      a.mainline.size must_== 42
    }

  "en passant e.p. notation" in:
    parse(enpassantEP) must beRight.like: a =>
      a.mainline.size must_== 36

    parse(enpassantEP2) must beRight.like: a =>
      a.mainline.size must_== 36

  "en passant ep notation" in:
    parse(enpassantEP3) must beRight.like: a =>
      a.mainline.size must_== 5

  "year" in:
    "full date" in:
      parse(recentChessCom) must beRight.like { parsed =>
        parsed.tags.year must_== Some(2016)
      }
    "only year" in:
      parse(explorerPartialDate) must beRight.like { parsed =>
        parsed.tags.year must_== Some(1978)
      }

  "weird variant names" in:
    parse(stLouisFischerandom) must beRight.like { parsed =>
      parsed.tags.variant must_== Some(variant.Chess960)
    }

  "example from chessgames.com with weird comments" in:
    parse(chessgamesWeirdComments) must beRight

  "exotic notation from clono.no" in:
    parse(clonoNoExoticNotation) must beRight

  "example with tags & comments without moves 1" in:
    parse(tagsCommentsWithoutMoves1) must beRight

  "example with tags & comments without moves 2" in:
    parse(tagsCommentsWithoutMoves2) must beRight

  "game with comments" in:
    parse(gameWithComments) must beRight.like { a =>
      a.mainline.size must_== 106
    }

  "none break space" in:
    val nbsp = "1.  e4 e5"
    parse(nbsp) must beRight.like { a =>
      a.mainline.size must_== 2
    }
