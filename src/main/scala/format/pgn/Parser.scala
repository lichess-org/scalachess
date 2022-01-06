package chess
package format.pgn

import chess.variant.Variant
import cats.parse.{LocationMap, Numbers => N, Parser => P, Parser0 => P0, Rfc5234 => R}
import cats.data.Validated
import cats.data.Validated.{invalid, valid}
import cats.implicits._
import cats.parse.Parser.Expectation

// http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
object Parser {

  val whitespace = R.lf | R.wsp
  val whitespaces = whitespace.rep0.?

  def full(pgn: String): Validated[String, ParsedPgn] = {
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
      tagStr = splitted._1
      moveStr = splitted._2
      preTags <- TagParser(tagStr)
      parsedMoves <- MovesParser(moveStr)
      init = parsedMoves._1
      sans = Sans(parsedMoves._2)
      resultOption = parsedMoves._3
      tags = resultOption.filterNot(_ => preTags.exists(_.Result)).foldLeft(preTags)(_ + _)
    } yield ParsedPgn(init, tags, sans)
  }

  def moves(str: String, variant: Variant): Validated[String, Sans] =
    MovesParser.moves(str)

  def moves(strMoves: Iterable[String], variant: Variant): Validated[String, Sans] =
    strMoves.toList
      .traverse(MovesParser.move)
      .map(Sans(_))

  object MovesParser {

    private def cleanComments(comments: List[String]) = comments.map(_.trim).filter(_.nonEmpty)

    def apply(pgn: String): Validated[String, (InitialPosition, List[San], Option[Tag])] =
      strMoves.parseAll(pgn) match {
        case Right((init, moves, result)) =>
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
            case P.Error(0, _) => valid((InitialPosition(List()), List(), None))
            case _ => invalid(showExpectations("Cannot parse moves", pgn, err))
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

    val blockCommentary: P[String] = P.until0(P.char('}')).with1.between(P.char('{'), P.char('}'))

    val inlineCommentary: P[String] = P.char(';') *> P.until(R.lf)

    val commentary = (blockCommentary | inlineCommentary) <* whitespaces

    val result: P[String] = P.stringIn(List("*", "1/2-1/2", "½-½", "0-1", "1-0"))

    val nagGlyphsRE = P.stringIn(
      Glyph.PositionAssessment.all
        .map(_.symbol)
        .sortBy(-_.length)
    )

    val nag = (P.char('$') ~ R.digit.rep).string | nagGlyphsRE

    val nagGlyphs: P0[Glyphs] = (nag <* whitespaces).rep0.map(nags =>
      Glyphs fromList nags.flatMap {
        Glyph.find
      }
    )

    val moveExtras = commentary.as(())

    val positiveIntString: P[String] =
      (N.nonZeroDigit ~ N.digits0).string

    // '. ' or '... ' or '. ... '
    val numberSuffix = (P.char('.') | whitespace).rep0.void

    // 10. or 10... but not 0 or 1-0 or 1/2
    val number = (positiveIntString <* !P.charIn('-', '/') ~ numberSuffix).string

    val forbidNullMove =
      P.stringIn(List("--", "Z0", "null", "pass", "@@@@")).?.flatMap(o => o.fold(P.unit)(_ => P.failWith("Lichess does not support null moves").void))

    val strMove: P[San] = P
      .recursive[San] { recuse =>
        val variation: P[Sans] =
          (((P.char('(') <* whitespaces) *> recuse.rep0 <* (P.char(')') ~ whitespaces)) <* whitespaces)
            .map(Sans(_))

        ((number.backtrack | commentary <* whitespaces).rep0 ~ forbidNullMove).with1 *>
          (((MoveParser.moveWithSuffix ~ nagGlyphs ~ commentary.rep0 ~ nagGlyphs ~ variation.rep0) <* moveExtras.rep0) <* whitespaces).backtrack
            .map { case ((((san, glyphs), comments), glyphs2), variations) =>
              san withComments comments withVariations variations mergeGlyphs (glyphs merge glyphs2)
            }
      }

    val strMoves: P[(InitialPosition, List[San], Option[String])] =
      ((commentary.rep0.with1 ~ strMove.rep) ~ (result <* whitespaces).? ~ commentary.rep0).map {
        case (((coms, sans), res), _) => (InitialPosition(cleanComments(coms)), sans.toList, res)
      }

  }

  object MoveParser {

    def rangeToMap(r: Iterable[Char]) = r.zipWithIndex.to(Map).view.mapValues(_ + 1)

    val fileMap = rangeToMap('a' to 'h')
    val rankMap = rangeToMap('1' to '8')

    val qCastle: P[Side] = P.stringIn(List("O-O-O", "o-o-o", "0-0-0")).as(QueenSide)

    val kCastle: P[Side] = P.stringIn(List("O-O", "o-o", "0-0")).as(KingSide)

    val glyph: P[Glyph] =
      mapParser(
        Glyph.MoveAssessment.all.sortBy(_.symbol.length).map { g =>
          g.symbol -> g
        },
        "glyph"
      )

    val glyphs = glyph.rep0.map(gs => Glyphs.fromList(gs))

    val x = P.char('x').?.map(_.isDefined)

    val check = P.char('+').?.map(_.isDefined)

    val checkmate = (P.char('#') | P.string("++")).?.map(_.isDefined)

    val role = mapParser(Role.allByPgn, "role")

    val file = mapParser(fileMap, "file")

    val rank = mapParser(rankMap, "rank")

    val dest: P[Pos] = mapParser(Pos.allKeys, "dest")

    val promotable = Role.allPromotableByPgn mapKeys (_.toUpper)

    val promotion: P[PromotableRole] = P.char('=').?.with1 *> mapParser(promotable, "promotion")

    // e5
    val pawn: P[Std] = dest.map(Std(_, Pawn))

    // Bg5
    val ambigous: P[Std] = (role ~ x ~ dest).map { case ((ro, ca), de) =>
      Std(dest = de, role = ro, capture = ca)
    }

    // B@g5
    val drop: P[Drop] = ((role <* P.char('@')) ~ dest).map { case (role, pos) => Drop(role, pos) }

    val pawnDrop: P[Drop] = (P.char('@') *> dest).map(Drop(Pawn, _))

    // Bac3 Baxc3 B2c3 B2xc3 Ba2xc3
    val disambiguated: P[Std] = (role ~ file.? ~ rank.? ~ x ~ dest).map { case ((((ro, fi), ra), ca), de) =>
      Std(dest = de, role = ro, capture = ca, file = fi, rank = ra)
    }

    // d7d5
    val disambiguatedPawn: P[Std] = (((file.? ~ rank.?) ~ x).with1 ~ dest).map { case (((fi, ra), ca), de) =>
      Std(dest = de, role = Pawn, capture = ca, file = fi, rank = ra)
    }

    val suffixes: P0[Suffixes] = (promotion.? ~ checkmate ~ check ~ glyphs).map { case (((p, cm), c), g) =>
      Suffixes(c, cm, p, g)
    }

    val castle: P[San] = (qCastle | kCastle).map(Castle(_))

    val standard: P[San] =
      disambiguatedPawn.backtrack | pawn.backtrack | disambiguated.backtrack | ambigous.backtrack | drop.backtrack | pawnDrop.backtrack | P.failWith("Invalid chess move")

    val move: P[San] = (castle | standard).withContext("Invalid chess move")
    val moveWithSuffix: P[San] = (move ~ suffixes <* whitespaces)
      .map { case (std, suf) =>
        std withSuffixes suf
      }

    def apply(str: String): Validated[String, San] =
      moveWithSuffix.parse(str) match {
        case Right((_, san)) => valid(san)
        case Left(err) => invalid("Cannot parse move: %s\n%s".format(err.toString, str))
      }

    def mapParser[A, B](pairs: Iterable[(A, B)], name: String): P[B] =
      pairs.foldLeft(P.failWith(name + " not found"): P[B]) { case (acc, (a, b)) =>
        P.string(a.toString).as(b) | acc
      }

  }

  object TagParser {

    val tagName: P[String] = R.alpha.rep.string
    val escapeDquote: P[String] = (P.char('\\') ~ R.dquote).as("\"")
    val valueChar: P[String] = escapeDquote | P.charWhere(_ != '"').string
    val tagValue: P[String] = valueChar.rep0.map(_.mkString).with1.surroundedBy(R.dquote)
    val tagContent: P[Tag] = ((tagName <* R.wsp.rep) ~ tagValue).map(p => Tag(p._1, p._2))
    val tag: P[Tag] = tagContent.between(P.char('['), P.char(']')) <* whitespace.rep0
    val tags: P[Tags] = tag.backtrack.rep.map(tags => Tags(tags.toList))

    def apply(pgn: String): Validated[String, Tags] =
      tags.parse(pgn) match {
        case Left(err) =>
          err match {
            // TODO when error happen at the first character return empty Tags, because it is fed with empty string sometime.
            case P.Error(0, _) => valid(Tags(List()))
            case _ => invalid("Cannot parse tags: %s\n%s".format(err.toString, pgn))
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

  private def showExpectations(context: String, str: String, error: P.Error): String = {
    val lm = LocationMap(str)
    val idx = error.failedAtOffset
    val caret = lm.toCaret(idx).getOrElse {
      throw new RuntimeException("This is impossible")
    }
    val line = lm.getLine(caret.line).getOrElse("")
    val errorLine = lm.getLine(caret.line).getOrElse("") ++ "\n" ++ " ".repeat(caret.col) ++ "^"
    //    val errorMessage = error.expected.map(expToString).mkString_("\n")
    val errorMessage = s"$context: [${caret.line + 1}.${caret.col + 1}]: ${expToString(error.expected.head)}"
    //    s"failed at line ${caret.line}, column ${caret.col}, offset: ${caret.offset} because of \n" ++ error.expected.map(expToString).mkString_("\n") ++ "\n" ++ str
    errorMessage ++ "\n" ++ errorLine ++ "\n" ++ str
  }

  private def expToString(expectation: Expectation): String =
    expectation match {
      case Expectation.OneOfStr(offset, strs) => strs match {
        case one :: Nil => s"at $offset expected: $one"
        case _ => s"at $offset expected one of: $strs"
      }
      case Expectation.InRange(offset, lower, upper) => s"expected char in range: [$lower, $upper]"
      case Expectation.StartOfString(offset) => "expected start of the file"
      case Expectation.EndOfString(offset, length) => s"expected end of file but $length characters remaining"
      case Expectation.Length(offset, expected, actual) => s"expected $expected more characters but only $actual remaining"
      case Expectation.ExpectedFailureAt(offset, matched) => s"expected failure but the parser matched: $matched"
      case Expectation.Fail(offset) => "Failed"
      case Expectation.FailWith(offset, message) => message
      case Expectation.WithContext(contextStr, expect) => contextStr
    }
}
