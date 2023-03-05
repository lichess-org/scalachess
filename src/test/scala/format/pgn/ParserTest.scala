package chess
package format.pgn

import cats.syntax.option.*
import scala.language.implicitConversions
import Sans.*

class ParserTest extends ChessTest:

  import Fixtures.*

  val parser = Parser.full

  def parseMove(s: String) = Parser.move(SanStr(s))

  "bom header" should {
    "be ignored" in {
      "with tags" in {
        parser("\uFEFF[Event \"Some event\"]\n1. e4 e5") must beValid.like { case parsed =>
          parsed.tags(_.Event) must_== Some("Some event")
          parsed.sans.value.size must_== 2
        }
      }
      "without tags" in {
        parser("\uFEFF1. e4 e5 3. Nf3") must beValid.like { case parsed =>
          parsed.sans.value.size must_== 3
        }
      }
    }
  }

  "pgnComment" should {
    "parse valid comment" in {
      Parser.pgnComment.parse("% comment") must beRight
    }
    "parse invalid comment" in {
      Parser.pgnComment.parse("  %comment") must beLeft
    }
  }

  "promotion check" should {
    "as a queen" in {
      parser("b8=Q ") must beValid { (parsed: ParsedPgn) =>
        parsed.sans.value.headOption must beSome { (san: San) =>
          san.asInstanceOf[Std].promotion == Option(Queen)
        }
      }
    }
    "as a rook" in {
      parser("b8=R ") must beValid { (parsed: ParsedPgn) =>
        parsed.sans.value.headOption must beSome { (san: San) =>
          san.asInstanceOf[Std].promotion must_== Option(Rook)
        }
      }
    }
  }

  "carriage return" in {
    "none" in {
      parser("1. e4 c6\n2. d4 d5") must beValid.like { case parsed =>
        parsed.sans.value.size must_== 4
      }
    }
    "one" in {
      parser("1. e4 c6\r\n2. d4 d5") must beValid.like { case parsed =>
        parsed.sans.value.size must_== 4
      }
    }
    "two" in {
      parser("1. e4 c6\r\r\n2. d4 d5") must beValid.like { case parsed =>
        parsed.sans.value.size must_== 4
      }
    }
    "between tags" in {
      parser("[White \"carriage\"]\r\n[Black \"return\"]\r\n\r\n1. a3 a6\r\n") must beValid.like {
        case parsed =>
          parsed.tags(_.White) must_== Some("carriage")
          parsed.tags(_.Black) must_== Some("return")
          parsed.sans.value.size must_== 2
      }
    }
  }

  "result" in {
    "no tag but inline result" in {
      parser(noTagButResult) must beValid.like { case parsed =>
        parsed.tags("Result") must_== Option("1-0")
      }
    }
    "in tags" in {
      parser(whiteResignsInTags) must beValid.like { case parsed =>
        parsed.tags("Result") must_== Option("0-1")
      }
    }
    "in moves" in {
      parser(whiteResignsInMoves) must beValid.like { case parsed =>
        parsed.tags("Result") must_== Option("0-1")
      }
    }
    "in tags and moves" in {
      parser(whiteResignsInTagsAndMoves) must beValid.like { case parsed =>
        parsed.tags("Result") must_== Option("0-1")
      }
    }
  }

  "glyphs" in {
    parseMove("e4") must beValid.like { case a =>
      a must_== Std(Pos.E4, Pawn)
    }
    parseMove("e4!") must beValid.like { case a: Std =>
      a.dest === Pos.E4
      a.role === Pawn
      a.metas.glyphs === Glyphs(Glyph.MoveAssessment.good.some, None, Nil)
    }
    parseMove("Ne7g6+?!") must beValid.like { case a: Std =>
      a.dest === Pos.G6
      a.role === Knight
      a.metas.glyphs === Glyphs(Glyph.MoveAssessment.dubious.some, None, Nil)
    }
    parser("Ne7g6+!") must beValid
    parseMove("P@e4?!") must beValid.like { case a: Drop =>
      a.pos === Pos.E4
      a.role === Pawn
      a.metas.glyphs === Glyphs(Glyph.MoveAssessment.dubious.some, None, Nil)
    }
  }
  extension (sans: Sans) def head = sans.value.head

  "nags" in {
    parser(withNag) must beValid

    parser("Ne7g6+! $13") must beValid.like { case ParsedPgn(_, _, sans) =>
      sans.head.metas.glyphs.move must_== Option(Glyph.MoveAssessment.good)
      sans.head.metas.glyphs.position must_== Option(Glyph.PositionAssessment.unclear)
    }
  }

  "non-nags" in {
    parser(withGlyphAnnotations) must beValid

    parser("Bxd3?? ∞") must beValid.like { case ParsedPgn(_, _, sans) =>
      sans.head.metas.glyphs.move must_== Option(Glyph.MoveAssessment.blunder)
      sans.head.metas.glyphs.position must_== Option(Glyph.PositionAssessment.unclear)
    }
  }

  "comments" in {
    parser("Ne7g6+! {such a neat comment}") must beValid.like { case ParsedPgn(_, _, sans) =>
      sans.head.metas.comments must_== List("such a neat comment")
    }
  }

  "variations" in {
    parser("Ne7g6+! {such a neat comment} (e4 Ng6)") must beValid.like { case ParsedPgn(_, _, sans) =>
      sans.head.metas.variations.headOption must beSome {
        (_: Sans).value must haveSize(2)
      }
    }
  }

  "first move variation" in {
    parser("1. e4 (1. d4)") must beValid.like { case ParsedPgn(_, _, sans) =>
      sans.head.metas.variations.headOption must beSome {
        (_: Sans).value must haveSize(1)
      }
    }
  }

  raws foreach { sans =>
    val size = sans.split(' ').length
    "sans only size: " + size in {
      parser(sans) must beValid.like { case a =>
        a.sans.value.size must_== size
      }
    }
  }

  (shortCastles ++ longCastles ++ annotatedCastles) foreach { sans =>
    val size = sans.split(' ').length
    "sans only size: " + size in {
      parser(sans) must beValid.like { case a =>
        a.sans.value.size must_== size
      }
    }
  }

  "disambiguated" in {
    parser(disambiguated) must beValid.like { case a =>
      a.sans.value.size must_== 3
    }
  }

  List(fromProd1, fromProd2, castleCheck1, castleCheck2) foreach { sans =>
    val size = sans.split(' ').length
    "sans only from prod size: " + size in {
      parser(sans) must beValid.like { case a =>
        a.sans.value.size must_== size
      }
    }
  }

  "variations" in {
    parser(variations) must beValid.like { case a =>
      a.sans.value.size must_== 20
    }
  }

  "inline tags" in {
    parser(inlineTags) must beValid.like { case a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.White && tag.value == "Blazquez, Denis"
      }
    }
  }

  "tag with nested quotes" in {
    parser("""[Black "Schwarzenegger, Arnold \"The Terminator\""]""") must beValid.like { case a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Black && tag.value == """Schwarzenegger, Arnold "The Terminator""""
      }
    }
  }

  "tag with inner brackets" in {
    parser("""[Black "[=0040.34h5a4]"]""") must beValid.like { case a =>
      a.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Black && tag.value == "[=0040.34h5a4]"
      }
    }
  }

  "game from wikipedia" in {
    parser(fromWikipedia) must beValid.like { case a =>
      a.sans.value.size must_== 85
    }
  }

  "game from crafty" in {
    parser(fromCrafty) must beValid.like { case a =>
      a.sans.value.size must_== 68
    }
  }

  "inline comments" in {
    parser(inlineComments) must beValid.like { case a =>
      a.sans.value.size must_== 85
    }
  }

  "comments and variations" in {
    parser(commentsAndVariations) must beValid.like { case a =>
      a.sans.value.size must_== 103
    }
  }

  "comments and lines by smartchess" in {
    parser(bySmartChess) must beValid.like { case a =>
      a.sans.value.size must_== 65
    }
  }

  "complete 960" in {
    parser(complete960) must beValid.like { case a =>
      a.sans.value.size must_== 42
    }
  }

  "TCEC" in {
    parser(fromTcec) must beValid.like { case a =>
      a.sans.value.size must_== 142
    }
  }

  "TCEC with engine output" in {
    parser(fromTcecWithEngineOutput) must beValid.like { case a =>
      a.sans.value.size must_== 165
    }
  }

  "chesskids iphone" in {
    parser(chesskids) must beValid.like { case a =>
      a.sans.value.size must_== 135
    }
  }

  "handwritten" in {
    parser(handwritten) must beValid.like { case a =>
      a.sans.value.size must_== 139
    }
  }

  "chess by post" in {
    parser(chessByPost) must beValid.like { case a =>
      a.sans.value.size must_== 100
    }
  }

  "Android device" in {
    parser(android) must beValid.like { case a =>
      a.sans.value.size must_== 69
    }
  }

  "weird dashes" in {
    parser(weirdDashes) must beValid.like { case a =>
      a.sans.value.size must_== 74
    }
  }

  "lichobile" in {
    parser(lichobile) must beValid.like { case a =>
      a.sans.value.size must_== 68
    }
  }

  "overflow" in {
    parser(overflow) must beValid.like { case a =>
      a.sans.value.size must_== 67
    }
  }
  "overflow 2" in {
    parser(stackOverflow) must beValid.like { case a =>
      a.sans.value.size must_== 8
    }
  }
  "overflow 3" in {
    parser(overflow3) must beValid.like { case a =>
      a.sans.value.size must_== 343
    }
  }
  "overflow 3: tags" in {
    parser(overflow3) must beValid.like { case a =>
      a.tags.value.size must_== 9
    }
  }
  "chessbase arrows" in {
    parser(chessbaseArrows) must beValid.like { case a =>
      a.initialPosition.comments must_== List(
        "[%csl Gb4,Yd5,Rf6][%cal Ge2e4,Ye2d4,Re2g4]"
      )
    }
  }
  "chessbase weird" in {
    parser(chessbaseWeird) must beValid.like { case a =>
      a.sans.value.size must_== 115
    }
  }
  "crazyhouse from prod" in {
    parser(crazyhouseFromProd) must beValid.like { case a =>
      a.sans.value.size must_== 49
    }
  }
  "crazyhouse from chess.com" in {
    parser(chessComCrazyhouse) must beValid.like { case a =>
      a.sans.value.size must_== 42
    }
  }
  "en passant e.p. notation" in {
    parser(enpassantEP) must beValid.like { case a =>
      a.sans.value.size must_== 36
    }
    parser(enpassantEP2) must beValid.like { case a =>
      a.sans.value.size must_== 36
    }
  }

  "year" in {
    "full date" in {
      parser(recentChessCom) must beValid.like { case parsed =>
        parsed.tags.year must_== Option(2016)
      }
    }
    "only year" in {
      parser(explorerPartialDate) must beValid.like { case parsed =>
        parsed.tags.year must_== Option(1978)
      }
    }
  }

  "weird variant names" in {
    parser(stLouisFischerandom) must beValid.like { case parsed =>
      parsed.tags.variant must_== Option(variant.Chess960)
    }
  }

  "example from chessgames.com with weird comments" in {
    parser(chessgamesWeirdComments) must beValid
  }

  "exotic notation from clono.no" in {
    parser(clonoNoExoticNotation) must beValid
  }

  "example with tags & comments without moves 1" in {
    parser(tagsCommentsWithoutMoves1) must beValid
  }

  "example with tags & comments without moves 2" in {
    parser(tagsCommentsWithoutMoves2) must beValid
  }

  "game with comments" in {
    parser(gameWithComments) must beValid.like { case a =>
      a.sans.value.size must_== 106
    }
  }

  "none break space" in {
    val nbsp = "1.  e4 e5"
    parser(nbsp) must beValid.like { case a =>
      a.sans.value.size must_== 2
    }
  }
