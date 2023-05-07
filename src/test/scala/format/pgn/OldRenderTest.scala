package chess
package format.pgn

import cats.syntax.option.*
import scala.language.implicitConversions
import chess.Node as CNode

case class TestPgn(tags: Tags, turns: List[Turn], initial: Initial = Initial.empty):

  // index is NOT a full move turn!
  private def updateTurnAt(index: Int, f: Turn => Turn) =
    // val index = fullMove.value - 1
    (turns lift index).fold(this) { turn =>
      copy(turns = turns.updated(index, f(turn)))
    }

  def updatePly(ply: Ply, f: TestMove => TestMove) =
    updateTurnAt((ply + 1).value / 2 - 1, _.update(!ply.color, f))

  def updateLastPly(f: TestMove => TestMove) = updatePly(Ply(nbPlies), f)

  def nbPlies = turns.foldLeft(0)(_ + _.count)

  def moves: List[TestMove] =
    turns.flatMap { t =>
      List(t.white, t.black).flatten
    }

  def withEvent(title: String) =
    copy(tags = tags + Tag(_.Event, title))

  def render: PgnStr = PgnStr:
    val initStr =
      if (initial.comments.nonEmpty) initial.comments.mkString("{ ", " } { ", " }\n")
      else ""
    val turnStr   = turns mkString " "
    val resultStr = tags(_.Result) | ""
    val endStr =
      if (turnStr.nonEmpty) s" $resultStr"
      else resultStr
    s"$tags\n\n$initStr$turnStr$endStr".trim

  override def toString = render.value

case class Turn(
    number: Int,
    white: Option[TestMove],
    black: Option[TestMove]
):

  def update(color: Color, f: TestMove => TestMove) =
    color.fold(
      copy(white = white map f),
      copy(black = black map f)
    )

  def updateLast(f: TestMove => TestMove) = {
    black.map(m => copy(black = f(m).some)) orElse
      white.map(m => copy(white = f(m).some))
  } | this

  def isEmpty = white.isEmpty && black.isEmpty

  def plyOf(color: Color) = Ply(number * 2 - color.fold(2, 1))

  def count = List(white, black) count (_.isDefined)

  override def toString =
    val text = (white, black) match
      case (Some(w), Some(b)) if w.isLong => s" $w $number... $b"
      case (Some(w), Some(b))             => s" $w $b"
      case (Some(w), None)                => s" $w"
      // the begin of a variation
      case (None, Some(b)) => s".. $b"
      case _               => ""
    s"$number.$text"

object Turn:

  def fromMoves(moves: List[TestMove], ply: Ply): List[Turn] = {
    moves.foldLeft((List.empty[Turn], ply)):
      case ((turns, p), move) if (p.value & 1) == 1 =>
        (Turn((p.value + 1) / 2, move.some, none) :: turns) -> (p + 1)
      case ((Nil, p), move) =>
        (Turn((p.value + 1) / 2, none, move.some) :: Nil) -> (p + 1)
      case ((t :: tt, p), move) =>
        (t.copy(black = move.some) :: tt) -> (p + 1)
  }._1.reverse

case class TestMove(
    san: SanStr,
    comments: List[Comment] = Nil,
    glyphs: Glyphs = Glyphs.empty,
    opening: Option[String] = None,
    result: Option[String] = None,
    variations: List[List[Turn]] = Nil,
    // time left for the user who made the move, after he made it
    secondsLeft: Option[Int] = None
):

  def isLong = comments.nonEmpty || variations.nonEmpty || secondsLeft.isDefined

  private def clockString: Option[String] =
    secondsLeft.map(seconds => "[%clk " + Move.formatPgnSeconds(seconds) + "]")

  override def toString =
    val glyphStr = glyphs.toList.map {
      case glyph if glyph.id <= 6 => glyph.symbol
      case glyph                  => s" $$${glyph.id}"
    }.mkString
    val commentsOrTime =
      if (comments.nonEmpty || secondsLeft.isDefined || opening.isDefined || result.isDefined)
        List(clockString, opening, result).flatten
          .:::(comments.map(_ map Move.noDoubleLineBreak))
          .map { text =>
            s" { $text }"
          }
          .mkString
      else ""
    val variationString =
      if (variations.isEmpty) ""
      else variations.map(_.mkString(" (", " ", ")")).mkString(" ")
    s"$san$glyphStr$commentsOrTime$variationString"

object NewPgn:
  def moves(turn: Turn): List[(Ply, TestMove)] =
    List(
      turn.white.map(m => turn.plyOf(White) -> m),
      turn.black.map(m => turn.plyOf(Black) -> m)
    ).flatten
  def moves(pgn: TestPgn): List[(Ply, TestMove)] = pgn.turns.flatMap(moves)

  extension (move: TestMove)
    def clean(ply: Ply): Move = Move(
      ply = ply,
      san = move.san,
      comments = move.comments,
      glyphs = move.glyphs,
      opening = move.opening,
      result = move.result,
      secondsLeft = move.secondsLeft
    )

  def apply(pgn: TestPgn): Pgn =
    val tree = moves(pgn).reverse.foldLeft(none[PgnTree]) { (o, move) => Some(toNode(move._2, move._1, o)) }
    Pgn(tags = pgn.tags, initial = pgn.initial, tree = tree)

  def toNode(move: TestMove, ply: Ply, child: Option[PgnTree]): PgnTree =
    CNode(
      move.clean(ply),
      child,
      move.variations.map(_.flatMap(moves)).map(x => toVariation(x.head._2, ply, None))
    )

  def toVariation(move: TestMove, ply: Ply, child: Option[PgnTree]): Variation[Move] =
    Variation(
      move.clean(ply),
      child
    )

class OldRenderTest extends ChessTest:

  given Conversion[String, SanStr]  = SanStr(_)
  given Conversion[String, Comment] = Comment(_)

  private def glyphs(id: Int) =
    Glyph.find(id).fold(Glyphs.empty) { g =>
      Glyphs fromList List(g)
    }

  /*
[Event "WCh"]
[Site "Bonn GER"]
[Date "2008.10.14"]
[Round "1"]
[White "Kramnik,V"]
[Black "Anand,V"]
[Result "1/2-1/2"]
[WhiteElo "2772"]
[BlackElo "2783"]
[ECO "D14"]
[Annotator "IM Malcolm Pein"]
[EventDate "2008.10.14"]

{ It wasn't a riveting start but you don't get many risks taken in game one
when the score is still level. Kramnik asked a question, Anand answered
confidently }

1. d4 d5 2. c4 c6 3. Nc3 Nf6 4. cxd5 { The Exchange Slav, the sure way to
play with zero losing chances so an ideal choice for game one } 4... cxd5
5. Bf4 Nc6 6. e3 Bf5 7. Nf3 e6 { Black cannot continue symmetrically for
too long of course but this is the most solid choice } 8. Qb3 Bb4 9. Bb5
O-O { Black breaks the symmetry but this is still the main line of chess
opening theory } 10. Bxc6 (10. O-O Bxc3 11. Bxc6 Bxb2 12. Bxb7 Bxa1 13.
   */

  "PGN string output" should:
    "be correct when there are no move times" in:
      val pgn = TestPgn(
        tags = Tags(
          List(
            Tag(_.White, "Kramnik,V"),
            Tag(_.Black, "Anand,V"),
            Tag(_.ECO, "D14")
          )
        ),
        turns = List(
          Turn(
            number = 1,
            white = TestMove("d4").some,
            black = TestMove("d5").some
          ),
          Turn(
            number = 2,
            white = TestMove("c4", glyphs = glyphs(1)).some,
            black = TestMove("c6", glyphs = glyphs(2)).some
          ),
          Turn(
            number = 3,
            white = TestMove("Nc3", glyphs = glyphs(3)).some,
            black = TestMove("Nf6").some
          ),
          Turn(
            number = 4,
            white = TestMove(
              "cxd5",
              comments =
                "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one" :: Nil
            ).some,
            black = TestMove("cxd5").some
          ),
          Turn(
            number = 5,
            white = TestMove("Bf4").some,
            black = TestMove("Nc6").some
          )
        )
      )
      pgn.toString must_== """[White "Kramnik,V"]
[Black "Anand,V"]
[ECO "D14"]

1. d4 d5 2. c4! c6? 3. Nc3!! Nf6 4. cxd5 { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one } 4... cxd5 5. Bf4 Nc6"""
    "be correct when there are move times" in:
      val pgn = TestPgn(
        tags = Tags(
          List(
            Tag(_.White, "tsinnema"),
            Tag(_.Black, "stockfish"),
            Tag(_.TimeControl, "300"),
            Tag(_.ECO, "A00e")
          )
        ),
        turns = List(
          Turn(
            number = 1,
            white = TestMove("a4", secondsLeft = 298.some).some,
            black = TestMove("Nf6", secondsLeft = 299.some).some
          ),
          Turn(
            number = 2,
            white = TestMove("d4", secondsLeft = 295.some).some,
            black = TestMove("d5", secondsLeft = 298.some).some
          ),
          Turn(
            number = 3,
            white = TestMove("h4", secondsLeft = 292.some).some,
            black = TestMove("e6", secondsLeft = 297.some).some
          ),
          Turn(
            number = 4,
            white = TestMove(
              "Qd3",
              glyphs = glyphs(1),
              secondsLeft = 288.some,
              comments = "An invention of true genius." :: Nil
            ).some,
            black = TestMove("c5", secondsLeft = 296.some).some
          ),
          Turn(
            number = 5,
            white = TestMove("dxc5", secondsLeft = 258.some).some,
            black = TestMove("Bxc5", glyphs = glyphs(1), secondsLeft = 295.some).some
          )
        )
      )
      pgn.toString must_== """[White "tsinnema"]
[Black "stockfish"]
[TimeControl "300"]
[ECO "A00e"]

1. a4 { [%clk 0:04:58] } 1... Nf6 { [%clk 0:04:59] } 2. d4 { [%clk 0:04:55] } 2... d5 { [%clk 0:04:58] } 3. h4 { [%clk 0:04:52] } 3... e6 { [%clk 0:04:57] } 4. Qd3! { An invention of true genius. } { [%clk 0:04:48] } 4... c5 { [%clk 0:04:56] } 5. dxc5 { [%clk 0:04:18] } 5... Bxc5! { [%clk 0:04:55] }"""

    "be correct with NAGs" in:
      val pgn = TestPgn(
        tags = Tags.empty,
        turns = List(
          Turn(
            number = 1,
            white = TestMove("d3", glyphs = glyphs(6)).some,
            black = TestMove("Nc6", glyphs = glyphs(10)).some
          ),
          Turn(
            number = 2,
            white = TestMove("Qd2").some,
            black = TestMove(
              "Nb4",
              glyphs = Glyphs(
                Glyph.MoveAssessment.blunder.some,
                Glyph.PositionAssessment.whiteMuchBetter.some,
                List(Glyph.Observation.timeTrouble)
              )
            ).some
          ),
          Turn(
            number = 3,
            white = TestMove("Qxb4", glyphs = glyphs(7)).some,
            black = None
          )
        )
      )
      pgn.toString must_== """1. d3?! Nc6 $10 2. Qd2 Nb4?? $18 $138 3. Qxb4 $7"""

    "be correct with variations" in:
      val pgn = TestPgn(
        tags = Tags.empty,
        turns = List(
          Turn(
            number = 1,
            white = TestMove(
              "d4",
              variations = List(
                List(
                  Turn(
                    number = 1,
                    white = TestMove("e4").some,
                    black = None
                  )
                )
              )
            ).some,
            black = TestMove(
              "Nf6",
              variations = List(
                List(
                  Turn(
                    number = 1,
                    white = None,
                    black = TestMove("d5").some
                  )
                )
              )
            ).some
          )
        )
      )
      pgn.toString must_== """1. d4 (1. e4) 1... Nf6 (1... d5)"""
    "handle Elon Musk-style baby names like [=0040.34h5a4] in tags" in:
      val pgn = TestPgn(
        tags = Tags(
          List(
            Tag(_.White, "tsinnema"),
            Tag(_.Black, "[=0040.34h5a4]"),
            Tag(_.TimeControl, "300"),
            Tag(_.ECO, "A00e")
          )
        ),
        turns = List(
          Turn(
            number = 1,
            white = TestMove("a4", secondsLeft = 298.some).some,
            black = TestMove("Nf6", secondsLeft = 299.some).some
          ),
          Turn(
            number = 2,
            white = TestMove("d4", secondsLeft = 295.some).some,
            black = TestMove("d5", secondsLeft = 298.some).some
          ),
          Turn(
            number = 3,
            white = TestMove("h4", secondsLeft = 292.some).some,
            black = TestMove("e6", secondsLeft = 297.some).some
          ),
          Turn(
            number = 4,
            white = TestMove(
              "Qd3",
              glyphs = glyphs(1),
              secondsLeft = 288.some,
              comments = "An invention of true genius." :: Nil
            ).some,
            black = TestMove("c5", secondsLeft = 296.some).some
          ),
          Turn(
            number = 5,
            white = TestMove("dxc5", secondsLeft = 258.some).some,
            black = TestMove("Bxc5", glyphs = glyphs(1), secondsLeft = 295.some).some
          )
        )
      )
      pgn.toString must_== """[White "tsinnema"]
[Black "[=0040.34h5a4]"]
[TimeControl "300"]
[ECO "A00e"]

1. a4 { [%clk 0:04:58] } 1... Nf6 { [%clk 0:04:59] } 2. d4 { [%clk 0:04:55] } 2... d5 { [%clk 0:04:58] } 3. h4 { [%clk 0:04:52] } 3... e6 { [%clk 0:04:57] } 4. Qd3! { An invention of true genius. } { [%clk 0:04:48] } 4... c5 { [%clk 0:04:56] } 5. dxc5 { [%clk 0:04:18] } 5... Bxc5! { [%clk 0:04:55] }"""
    "result only" in:
      val pgn = TestPgn(
        tags = Tags(
          List(
            Tag(_.Result, "0-1")
          )
        ),
        turns = List()
      )
      pgn.toString must_== """[Result "0-1"]

0-1"""

  "initial comments" should:
    "empty" in:
      val pgn = TestPgn(
        tags = Tags.empty,
        turns = List()
      )
      pgn.toString must_== """"""
    "empty with initial comment" in:
      val pgn = TestPgn(
        tags = Tags.empty,
        turns = List(),
        initial = Initial(List("Why hello there!"))
      )
      pgn.toString must_== """{ Why hello there! }"""
    "empty with initial comments" in:
      val pgn = TestPgn(
        tags = Tags.empty,
        turns = List(),
        initial = Initial(
          List(
            "Why hello there!",
            "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one"
          )
        )
      )
      pgn.toString must_== """{ Why hello there! } { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one }"""
    "moves with initial comments" in:
      val pgn = TestPgn(
        tags = Tags.empty,
        turns = List(
          Turn(
            number = 1,
            white = TestMove(
              "d4",
              variations = List(
                List(
                  Turn(
                    number = 1,
                    white = TestMove("e4").some,
                    black = None
                  )
                )
              )
            ).some,
            black = TestMove(
              "Nf6",
              variations = List(
                List(
                  Turn(
                    number = 1,
                    white = None,
                    black = TestMove("d5").some
                  )
                )
              )
            ).some
          )
        ),
        initial = Initial(
          List(
            "Why hello there!",
            "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one"
          )
        )
      )
      pgn.toString must_== """{ Why hello there! } { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one }
1. d4 (1. e4) 1... Nf6 (1... d5)"""

  "tags" should:
    "tag with \" in it" in:
      val pgn = TestPgn(
        tags = Tags(
          List(
            Tag(_.TimeControl, "\"")
          )
        ),
        turns = List()
      )
      pgn.toString must_== """[TimeControl "\""]"""

    "tag with \\ in it" in:
      val pgn = TestPgn(
        tags = Tags(
          List(
            Tag(_.TimeControl, "\\")
          )
        ),
        turns = List()
      )
      pgn.toString must_== """[TimeControl "\\"]"""
