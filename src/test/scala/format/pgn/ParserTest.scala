package chess
package format.pgn

import cats.syntax.option.*
import scala.language.implicitConversions
import Sans.*

class ParserTest extends ChessTest:

  import Fixtures.*

  given Conversion[SanStr, String] = _.value
  given Conversion[String, SanStr] = SanStr(_)

  val parse = Parser.full

  def parseMove(s: String) = Parser.move(s)

  extension (tree: Option[PgnNode[PgnNodeData]]) def head = tree.get.mainLine.head

  extension (parsed: NewParsedPgn) def metas = parsed.tree.get.move.metas

  "bom header" should:
    "be ignored" in:
      "with tags" in:
        parse("\uFEFF[Event \"Some event\"]\n1. e4 e5") must beValid.like { case parsed =>
          parsed.tags(_.Event) must_== Some("Some event")
          parsed.mainLine.size must_== 2
        }
      "without tags" in:
        parse("\uFEFF1. e4 e5 3. Nf3") must beValid.like { case parsed =>
          parsed.mainLine.size must_== 3
        }

  "pgnComment" should:
    "parse valid comment" in:
      Parser.pgnComment.parse("% comment") must beRight
    "parse invalid comment" in:
      Parser.pgnComment.parse("  %comment") must beLeft

  "promotion check" should:
    "as a queen" in:
      parse("b8=Q ") must beValid.like: parsed =>
        parsed.mainLine.headOption must beSome { (san: San) =>
          san === Std(Pos.B8, Pawn, promotion = Option(Queen))
        }

    "as a rook" in:
      parse("b8=R ") must beValid { (parsed: NewParsedPgn) =>
        parsed.mainLine.headOption must beSome { (san: San) =>
          san.asInstanceOf[Std].promotion must_== Option(Rook)
        }
      }

  "carriage return" in:
    "none" in:
      parse("1. e4 c6\n2. d4 d5") must beValid.like { case parsed =>
        parsed.mainLine.size must_== 4
      }
    "one" in:
      parse("1. e4 c6\r\n2. d4 d5") must beValid.like { case parsed =>
        parsed.mainLine.size must_== 4
      }
    "two" in:
      parse("1. e4 c6\r\r\n2. d4 d5") must beValid.like { case parsed =>
        parsed.mainLine.size must_== 4
      }
    "between tags" in:
      parse("[White \"carriage\"]\r\n[Black \"return\"]\r\n\r\n1. a3 a6\r\n") must beValid.like:
        case parsed =>
          parsed.tags(_.White) must_== Some("carriage")
          parsed.tags(_.Black) must_== Some("return")
          parsed.mainLine.size must_== 2

  "result" in:
    "no tag but inline result" in:
      parse(noTagButResult) must beValid.like { case parsed =>
        parsed.tags("Result") must_== Option("1-0")
      }
    "in tags" in:
      parse(whiteResignsInTags) must beValid.like { case parsed =>
        parsed.tags("Result") must_== Option("0-1")
      }
    "in moves" in:
      parse(whiteResignsInMoves) must beValid.like { case parsed =>
        parsed.tags("Result") must_== Option("0-1")
      }
    "in tags and moves" in:
      parse(whiteResignsInTagsAndMoves) must beValid.like { case parsed =>
        parsed.tags("Result") must_== Option("0-1")
      }

  "glyphs" in:

    parseMove("b8=B ") must beValid.like: node =>
      node.move.san === Std(Pos.B8, Pawn, promotion = Option(Bishop))

    parseMove("e4") must beValid.like: node =>
      node.move.san must_== Std(Pos.E4, Pawn)

    parseMove("e4") must beValid.like: node =>
      node.move.san must_== Std(Pos.E4, Pawn)

    parseMove("e4!") must beValid.like: node =>
      node.move.san === Std(Pos.E4, Pawn)
      node.move.metas.glyphs === Glyphs(Glyph.MoveAssessment.good.some, None, Nil)

    // TODO parsed result is off by one
    parseMove("Ne7g6+?!") must beValid.like: node =>
      node.move.san === Std(Pos.G6, Knight, false, Some(File.F), Some(Rank.Eighth))
      node.move.metas.glyphs === Glyphs(Glyph.MoveAssessment.dubious.some, None, Nil)

    parseMove("P@e4?!") must beValid.like: node =>
      node.move.san === Drop(Pawn, Pos.E4)
      node.move.metas.glyphs === Glyphs(Glyph.MoveAssessment.dubious.some, None, Nil)

  "nags" in:
    parse(withNag) must beValid

    parse("Ne7g6+! $13") must beValid.like: parsed =>
      parsed.metas.glyphs.move must_== Option(Glyph.MoveAssessment.good)
      parsed.metas.glyphs.position must_== Option(Glyph.PositionAssessment.unclear)

  "non-nags" in:
    parse(withGlyphAnnotations) must beValid

    parse("Bxd3?? ∞") must beValid.like { parsed =>
      parsed.tree.head.metas.glyphs.move must_== Option(Glyph.MoveAssessment.blunder)
      parsed.tree.head.metas.glyphs.position must_== Option(Glyph.PositionAssessment.unclear)
    }

  "comments" in:
    parse("Ne7g6+! {such a neat comment}") must beValid.like { parsed =>
      parsed.tree.head.metas.comments must_== List("such a neat comment")
    }

  "variations" in:
    parse("Ne7g6+! {such a neat comment} (e4 Ng6)") must beValid.like: parsed =>
      parsed.tree.get.variations.headOption must beSome:
        (_: ParsedPgnTree).mainLine must haveSize(2)

  "first move variation" in:
    parse("1. e4 (1. d4)") must beValid.like: parsed =>
      parsed.tree.get.variations.headOption must beSome:
        (_: ParsedPgnTree).mainLine must haveSize(1)

  raws foreach { sans =>
    val size = sans.split(' ').length
    "sans only size: " + size in:
      parse(sans) must beValid.like { a =>
        a.mainLine.size must_== size
      }
  }

  (shortCastles ++ longCastles ++ annotatedCastles) foreach { sans =>
    val size = sans.split(' ').length
    "sans only size: " + size in:
      parse(sans) must beValid.like: a =>
        a.mainLine.size must_== size
  }

  "disambiguated" in:
    parse(disambiguated) must beValid.like { case a =>
      a.mainLine.size must_== 3
    }

  List(fromProd1, fromProd2, castleCheck1, castleCheck2) foreach { sans =>
    val size = sans.split(' ').length
    "sans only from prod size: " + size in:
      parse(sans) must beValid.like { case a =>
        a.mainLine.size must_== size
      }
  }

  "variations" in:
    parse(variations) must beValid.like { case a =>
      a.mainLine.size must_== 20
    }

  "inline tags" in:
    parse(inlineTags) must beValid.like { case a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.White && tag.value == "Blazquez, Denis"
      }
    }

  "tag with nested quotes" in:
    parse("""[Black "Schwarzenegger, Arnold \"The Terminator\""]""") must beValid.like { case a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Black && tag.value == """Schwarzenegger, Arnold "The Terminator""""
      }
    }

  "tag with inner brackets" in:
    parse("""[Black "[=0040.34h5a4]"]""") must beValid.like { case a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Black && tag.value == "[=0040.34h5a4]"
      }
    }

  "game from wikipedia" in:
    parse(fromWikipedia) must beValid.like { case a =>
      a.mainLine.size must_== 85
    }

  "game from crafty" in:
    parse(fromCrafty) must beValid.like { case a =>
      a.mainLine.size must_== 68
    }

  "inline comments" in:
    parse(inlineComments) must beValid.like { case a =>
      a.mainLine.size must_== 85
    }

  "block comment in variation root" in:
    parse(rootCommentInVariation) must beValid.like: parsed =>
      parsed.tree.get.variations.head.move.variationComments must_==
        Some(List("This move:"))

  "inline comment in variation root" in:
    parse(rootCommentInVariation) must beValid.like: parsed =>
      parsed.tree.get.variations.tail.head.move.variationComments must_==
        Some(List("Neither does :"))

  "block comments in variation root" in:
    parse(multipleRootCommentsInVariation) must beValid.like: parsed =>
      parsed.tree.get.variations.head.move.variationComments must_==
        Some(List("This move:", "looks pretty"))

  "multiple comments in variation root" in:
    parse(multipleRootCommentsInVariation) must beValid.like: parsed =>
      parsed.tree.get.variations.tail.head.move.variationComments must_==
        Some(List("Neither does :", "this or that", "or whatever"))

  "comments and variations" in:
    parse(commentsAndVariations) must beValid.like: parsed =>
      parsed.mainLine.size must_== 103

  "comments and lines by smartchess" in:
    parse(bySmartChess) must beValid.like { case a =>
      a.mainLine.size must_== 65
    }

  "complete 960" in:
    parse(complete960) must beValid.like { case a =>
      a.mainLine.size must_== 42
    }

  "TCEC" in:
    parse(fromTcec) must beValid.like { case a =>
      a.mainLine.size must_== 142
    }

  "TCEC with engine output" in:
    parse(fromTcecWithEngineOutput) must beValid.like { case a =>
      a.mainLine.size must_== 165
    }

  "chesskids iphone" in:
    parse(chesskids) must beValid.like { case a =>
      a.mainLine.size must_== 135
    }

  "handwritten" in:
    parse(handwritten) must beValid.like { case a =>
      a.mainLine.size must_== 139
    }

  "chess by post" in:
    parse(chessByPost) must beValid.like { case a =>
      a.mainLine.size must_== 100
    }

  "Android device" in:
    parse(android) must beValid.like { case a =>
      a.mainLine.size must_== 69
    }

  "weird dashes" in:
    parse(weirdDashes) must beValid.like { case a =>
      a.mainLine.size must_== 74
    }

  "lichobile" in:
    parse(lichobile) must beValid.like { case a =>
      a.mainLine.size must_== 68
    }

  "overflow" in:
    parse(overflow) must beValid.like { case a =>
      a.mainLine.size must_== 67
    }
  "overflow 2" in:
    parse(stackOverflow) must beValid.like { case a =>
      a.mainLine.size must_== 8
    }
  "overflow 3" in:
    parse(overflow3) must beValid.like { case a =>
      a.mainLine.size must_== 343
    }
  "overflow 3: tags" in:
    parse(overflow3) must beValid.like { case a =>
      a.tags.value.size must_== 9
    }
  "chessbase arrows" in:
    parse(chessbaseArrows) must beValid.like { case a =>
      a.initialPosition.comments must_== List(
        "[%csl Gb4,Yd5,Rf6][%cal Ge2e4,Ye2d4,Re2g4]"
      )
    }
  "multiple initial comments with empty" in:
    parse(multipleInitalCommentsWithEmpty) must beValid.like { case a =>
      a.initialPosition must_== List("this", "that")
    }
  "chessbase weird" in:
    parse(chessbaseWeird) must beValid.like { case a =>
      a.mainLine.size must_== 115
    }
  "crazyhouse from prod" in:
    parse(crazyhouseFromProd) must beValid.like { case a =>
      a.mainLine.size must_== 49
    }
  "crazyhouse from chess.com" in:
    parse(chessComCrazyhouse) must beValid.like { case a =>
      a.mainLine.size must_== 42
    }
  "en passant e.p. notation" in:
    parse(enpassantEP) must beValid.like { case a =>
      a.mainLine.size must_== 36
    }
    parse(enpassantEP2) must beValid.like { case a =>
      a.mainLine.size must_== 36
    }

  "year" in:
    "full date" in:
      parse(recentChessCom) must beValid.like { case parsed =>
        parsed.tags.year must_== Option(2016)
      }
    "only year" in:
      parse(explorerPartialDate) must beValid.like { case parsed =>
        parsed.tags.year must_== Option(1978)
      }

  "weird variant names" in:
    parse(stLouisFischerandom) must beValid.like { case parsed =>
      parsed.tags.variant must_== Option(variant.Chess960)
    }

  "example from chessgames.com with weird comments" in:
    parse(chessgamesWeirdComments) must beValid

  "exotic notation from clono.no" in:
    parse(clonoNoExoticNotation) must beValid

  "example with tags & comments without moves 1" in:
    parse(tagsCommentsWithoutMoves1) must beValid

  "example with tags & comments without moves 2" in:
    parse(tagsCommentsWithoutMoves2) must beValid

  "game with comments" in:
    parse(gameWithComments) must beValid.like { case a =>
      a.mainLine.size must_== 106
    }

  "none break space" in:
    val nbsp = "1.  e4 e5"
    parse(nbsp) must beValid.like { case a =>
      a.mainLine.size must_== 2
    }
