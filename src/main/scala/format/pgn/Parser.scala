package chess
package format.pgn

import chess.variant.Variant
import cats.parse.{ Numbers => N, Parser => P, Parser0 => P0, Rfc5234 => R }
import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

// http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
object Parser {

  case class StrMove(
      san: San,
      glyphs: Glyphs,
      comments: List[String],
      variations: List[List[StrMove]]
  ) {
    def toSan: San = san withComments comments withVariations {
      variations
        .map { v =>
          Sans(v.map(_.toSan))
        }
        .filter(_.value.nonEmpty)
    } mergeGlyphs glyphs
  }

  val whitespace  = R.lf | R.wsp
  val whitespaces = whitespace.rep0.?

  def full(pgn: String): Validated[String, ParsedPgn] =
    try {
      val preprocessed = augmentString(pgn).linesIterator
        .map(_.trim)
        .filterNot {
          _.headOption.contains('%')
        }
        .mkString("\n")
        .replace("[pgn]", "")
        .replace("[/pgn]", "")
        .replace("‑", "-")
        .replace("–", "-")
        .replace("e.p.", "") // silly en-passant notation
      for {
        splitted <- splitTagAndMoves(preprocessed)
        tagStr  = splitted._1
        moveStr = splitted._2
        preTags     <- TagParser(tagStr)
        parsedMoves <- MovesParser(moveStr)
        init         = parsedMoves._1
        sans         = Sans(parsedMoves._2)
        resultOption = parsedMoves._3
        tags         = resultOption.filterNot(_ => preTags.exists(_.Result)).foldLeft(preTags)(_ + _)
      } yield ParsedPgn(init, tags, sans)
    } catch {
      case _: StackOverflowError =>
        sys error "### StackOverflowError ### in PGN parser"
    }

  def moves(str: String, variant: Variant): Validated[String, Sans] =
    MovesParser.moves(str)

  def moves(strMoves: Iterable[String], variant: Variant): Validated[String, Sans] =
    strMoves.toList
      .traverse(MovesParser.move)
      .map(Sans(_))

  def objMoves(strMoves: List[StrMove], variant: Variant): Sans =
    Sans(strMoves.map(_.toSan))

  object MovesParser {

    private def cleanComments(comments: List[String]) = comments.map(_.trim).filter(_.nonEmpty)

    def apply(pgn: String): Validated[String, (InitialPosition, List[San], Option[Tag])] =
      strMoves.parse(pgn) match {
        case Right((_, (init, moves, result))) =>
          valid(
            (
              init,
              moves,
              result map { r =>
                Tag(_.Result, r)
              }
            )
          )
        case Left(err) =>
          err match {
            // TODO when error happen at the first character return empty Tags, because it is fed with empty string sometime.
            case P.Error(0, _) => valid((InitialPosition(List()), List(), None))
            case _             => invalid("Cannot parse moves: %s\n%s".format(err.toString, pgn))
          }
      }

    def moves(moves: String): Validated[String, Sans] =
      strMove.rep.map(xs => Sans(xs.toList)).parse(moves) match {
        case Right((_, str)) =>
          valid(str)
        case Left(err) => invalid("Cannot parse moves: %s\n%s".format(err.toString, moves))
      }

    def move(move: String): Validated[String, San] =
      strMove.parse(move) match {
        case Right((_, str)) =>
          valid(str)
        case Left(err) => invalid("Cannot parse move: %s\n%s".format(err.toString, move))
      }

    def whitespace: P[Unit] = R.lf | R.wsp

    def blockCommentary: P[String] = P.until0(P.char('}')).with1.between(P.char('{'), P.char('}'))

    def inlineCommentary: P[String] = P.char(';') *> P.until(R.lf)

    def commentary = (blockCommentary | inlineCommentary) <* whitespaces

    def result: P[String] = P.stringIn(List("*", "1/2-1/2", "½-½", "0-1", "1-0"))

    def nagGlyphsRE = P.stringIn(
      Glyph.PositionAssessment.all
        .map(_.symbol)
        .sortBy(-_.length)
    )

    def nag = (P.char('$') ~ R.digit.rep).string | nagGlyphsRE

    def nagGlyphs: P0[Glyphs] = (nag <* whitespaces).rep0.map(nags =>
      Glyphs fromList nags.flatMap {
        Glyph.find
      }
    )

    def moveExtras = commentary.as(())

    val positiveIntString: P[String] =
      (N.nonZeroDigit ~ N.digits0).string

    // '. ' or '... ' or '. ... '
    def numberSuffix = (P.char('.') | whitespace).rep0.void

    // 10. or 10... but not 0
    def number = (positiveIntString ~ numberSuffix).string

    def forbidNullMove =
      !P.stringIn(List("--", "Z0", "null", "pass", "@@@@")).withContext("Lichess does not support null moves")

    def strMoves: P[(InitialPosition, List[San], Option[String])] =
      ((commentary.rep0.with1 ~ strMove.rep) ~ result.? ~ commentary.rep0).map {
        case (((coms, sans), res), _) => (InitialPosition(cleanComments(coms)), sans.toList, res)
      }

    def strMove: P[San] = P
      .recursive[StrMove] { recuse =>
        def variation: P[List[StrMove]] =
          ((P.char('(') <* whitespaces) *> recuse.rep0 <* (P.char(')') ~ whitespaces)) <* whitespaces

        ((number.backtrack | commentary <* whitespaces).rep0 ~ forbidNullMove).with1.soft *>
          (((MoveParser.move ~ nagGlyphs ~ commentary.rep0 ~ nagGlyphs ~ variation.rep0) <* moveExtras.rep0) <* whitespaces).backtrack
            .map { case ((((san, glyphs), comments), glyphs2), variations) =>
              StrMove(san, glyphs merge glyphs2, cleanComments(comments), variations)
            }
      }
      .map(_.toSan)

  }

  object MoveParser {

    def rangeToMap(r: Iterable[Char]) = r.zipWithIndex.to(Map).view.mapValues(_ + 1)

    val fileMap = rangeToMap('a' to 'h')
    val rankMap = rangeToMap('1' to '8')

    def qCastle: P[Side] = P.stringIn(List("O-O-O", "o-o-o", "0-0-0")).as(QueenSide)

    def kCastle: P[Side] = P.stringIn(List("O-O", "o-o", "0-0")).as(KingSide)

    def glyph: P[Glyph] =
      mapParser(
        Glyph.MoveAssessment.all.sortBy(_.symbol.length).map { g =>
          g.symbol -> g
        },
        "glyph"
      )

    def glyphs = glyph.rep0.map(gs => Glyphs.fromList(gs))

    def x = P.char('x').?.map(_.isDefined)

    def check = P.char('+').?.map(_.isDefined)

    def checkmate = (P.char('#') | P.string("++")).?.map(_.isDefined)

    def role = mapParser(Role.allByPgn, "role")

    def file = mapParser(fileMap, "file")

    def rank = mapParser(rankMap, "rank")

    def dest: P[Pos] = mapParser(Pos.allKeys, "dest")

    def promotable = Role.allPromotableByPgn mapKeys (_.toUpper)

    def promotion: P[PromotableRole] = P.char('=').?.with1 *> mapParser(promotable, "promotion")

    // e5
    def pawn: P[Std] = dest.map(Std(_, Pawn))

    // Bg5
    def ambigous: P[Std] = (role ~ x ~ dest).map { case ((ro, ca), de) =>
      Std(dest = de, role = ro, capture = ca)
    }

    // B@g5
    def drop: P[Drop] = ((role <* P.char('@')) ~ dest).map { case (role, pos) => Drop(role, pos) }

    def pawnDrop: P[Drop] = (P.char('@') *> dest).map(Drop(Pawn, _))

    // Bac3 Baxc3 B2c3 B2xc3 Ba2xc3
    def disambiguated: P[Std] = (role ~ file.? ~ rank.? ~ x ~ dest).map { case ((((ro, fi), ra), ca), de) =>
      Std(dest = de, role = ro, capture = ca, file = fi, rank = ra)
    }

    // d7d5
    def disambiguatedPawn: P[Std] = (((file.? ~ rank.?) ~ x).with1 ~ dest).map { case (((fi, ra), ca), de) =>
      Std(dest = de, role = Pawn, capture = ca, file = fi, rank = ra)
    }

    def suffixes: P0[Suffixes] = (promotion.? ~ checkmate ~ check ~ glyphs).map { case (((p, cm), c), g) =>
      Suffixes(c, cm, p, g)
    }

    def castle: P[San] = ((qCastle | kCastle) ~ suffixes).map { case (side, suf) =>
      Castle(side) withSuffixes suf
    }

    def standard: P[San] =
      ((disambiguatedPawn.backtrack | pawn.backtrack | disambiguated.backtrack | ambigous.backtrack | drop.backtrack | pawnDrop.backtrack) ~ suffixes)
        .map { case (std, suf) =>
          std withSuffixes suf
        }

    def move = (castle | standard) <* whitespaces

    def apply(str: String): Validated[String, San] =
      move.parse(str) match {
        case Right((_, san)) => valid(san)
        case Left(err)       => invalid("Cannot parse move: %s\n%s".format(err.toString, str))
      }

    def mapParser[A, B](pairs: Iterable[(A, B)], name: String): P[B] =
      pairs.foldLeft(P.failWith(name + " not found"): P[B]) { case (acc, (a, b)) =>
        P.string(a.toString).as(b) | acc
      }

  }

  object TagParser {

    val tagName: P[String]      = R.alpha.rep.string
    val escapeDquote: P[String] = (P.char('\\') ~ R.dquote).as("\"")
    val valueChar: P[String]    = escapeDquote | P.charWhere(_ != '"').string
    val tagValue: P[String]     = valueChar.rep0.map(_.mkString).with1.surroundedBy(R.dquote)
    val tagContent: P[Tag]      = ((tagName <* R.wsp.rep) ~ tagValue).map(p => Tag(p._1, p._2))
    val tag: P[Tag]             = tagContent.between(P.char('['), P.char(']')) <* whitespace.rep0
    val tags: P[Tags]           = tag.backtrack.rep.map(tags => Tags(tags.toList))

    def apply(pgn: String): Validated[String, Tags] =
      tags.parse(pgn) match {
        case Left(err) =>
          err match {
            // TODO when error happen at the first character return empty Tags, because it is fed with empty string sometime.
            case P.Error(0, _) => valid(Tags(List()))
            case _             => invalid("Cannot parse tags: %s\n%s".format(err.toString, pgn))
          }
        case Right((_, tags)) => valid(tags)
      }

    def fromFullPgn(pgn: String): Validated[String, Tags] =
      splitTagAndMoves(pgn) flatMap { case (tags, _) =>
        apply(tags)
      }

  }

  // there must be a newline between the tags and the first move
  private def ensureTagsNewline(pgn: String): String =
    """"\]\s*(\d+\.)""".r.replaceAllIn(pgn, m => "\"]\n" + m.group(1))

  private def splitTagAndMoves(pgn: String): Validated[String, (String, String)] =
    augmentString(ensureTagsNewline(pgn)).linesIterator.to(List).map(_.trim).filter(_.nonEmpty) span { line =>
      line lift 0 contains '['
    } match {
      case (tagLines, moveLines) => valid(tagLines.mkString("\n") -> moveLines.mkString("\n"))
    }
}
