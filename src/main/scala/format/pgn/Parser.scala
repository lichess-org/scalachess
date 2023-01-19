package chess
package format.pgn

import cats.parse.{ LocationMap, Numbers as N, Parser as P, Parser0 as P0, Rfc5234 as R }
import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits.*
import cats.parse.Parser.Expectation
import cats.data.NonEmptyList

// http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
object Parser:

  val whitespace = R.cr | R.lf | R.wsp
  val pgnComment = P.caret.filter(_.col == 0) *> P.char('%') *> P.until(P.char('\n')).void
  // pgnComment with % or whitespaces
  val escape = pgnComment.? *> whitespace.rep0.?

  def full(pgn: String): Validated[String, ParsedPgn] =
    pgnParser.parse(pgn) match
      case Right((_, parsedResult)) =>
        valid(parsedResult)
      case Left(err) =>
        invalid(showExpectations("Cannot parse pgn", pgn, err))

  def moves(str: PgnMovesStr): Validated[String, Sans] =
    MovesParser.moves(str)

  def moves(strMoves: Iterable[SanStr]): Validated[String, Sans] =
    strMoves.toList
      .traverse(MovesParser.move)
      .map(Sans(_))

  def move(str: SanStr) = MovesParser.move(str)

  private object MovesParser:

    private def cleanComments(comments: List[String]) = comments.map(_.trim).filter(_.nonEmpty)

    def moves(str: PgnMovesStr): Validated[String, Sans] =
      strMove.rep.map(xs => Sans(xs.toList)).parse(str.value) match
        case Right((_, sans)) => valid(sans)
        case Left(err)        => invalid(showExpectations("Cannot parse moves", str.value, err))

    def move(str: SanStr): Validated[String, San] =
      strMove.parse(str.value) match
        case Right((_, san)) => valid(san)
        case Left(err)       => invalid(showExpectations("Cannot parse move", str.value, err))

    val blockCommentary: P[String] = P.until0(P.char('}')).with1.between(P.char('{'), P.char('}'))

    val inlineCommentary: P[String] = P.char(';') *> P.until(R.lf)

    val commentary = (blockCommentary | inlineCommentary).withContext("Invalid comment") <* escape
    val resultList = List(
      "*",
      "1/2-1/2",
      "½-½",
      "0-1",
      "1-0",
      "1/2‑1/2",
      "½‑½",
      "0‑1",
      "1‑0",
      "1/2–1/2",
      "½–½",
      "0–1",
      "1–0"
    )

    def mapResult(result: String): String = result match
      case "½-½"     => "1/2-1/2"
      case "1/2‑1/2" => "1/2-1/2"
      case "½‑½"     => "1/2-1/2"
      case "1/2–1/2" => "1/2-1/2"
      case "½–½"     => "1/2-1/2"
      case "0‑1"     => "0-1"
      case "0–1"     => "0-1"
      case "1‑0"     => "1-0"
      case "1–0"     => "1-0"
      case x         => x

    val result: P[String] = P.stringIn(resultList).map(mapResult)

    val nagGlyphsRE = P.stringIn(
      Glyph.PositionAssessment.all
        .map(_.symbol)
        .sortBy(-_.length)
    )

    val nag = (P.char('$') ~ R.digit.rep).string | nagGlyphsRE

    val nagGlyphs: P0[Glyphs] = (nag <* escape).rep0.map(nags =>
      Glyphs fromList nags.flatMap {
        Glyph.find
      }
    )

    val moveExtras = commentary.void

    val positiveIntString: P[String] =
      (N.nonZeroDigit ~ N.digits0).string

    // '. ' or '... ' or '. ... '
    val numberSuffix = (P.char('.') | whitespace).rep0.void

    // 10. or 10... but not 0 or 1-0 or 1/2
    val number = (positiveIntString <* !P.charIn('‑', '–', '-', '/') ~ numberSuffix).string

    val forbidNullMove =
      P.stringIn(List("--", "Z0", "null", "pass", "@@@@"))
        .?
        .flatMap(o => o.fold(P.unit)(_ => P.failWith("Lichess does not support null moves").void))

    val strMove: P[San] = P
      .recursive[San] { recuse =>
        val variation: P[Sans] =
          (((P.char('(') <* escape) *> recuse.rep0 <* (P.char(')') ~ escape)) <* escape)
            .map(Sans(_))

        ((number.backtrack | (commentary <* escape)).rep0 ~ forbidNullMove).with1 *>
          (((MoveParser.moveWithSuffix ~ nagGlyphs ~ commentary.rep0 ~ nagGlyphs ~ variation.rep0) <* moveExtras.rep0) <* escape).backtrack
            .map { case ((((san, glyphs), comments), glyphs2), variations) =>
              san withComments comments withVariations variations mergeGlyphs (glyphs merge glyphs2)
            }
      }

    val strMoves: P0[(InitialPosition, List[San], Option[String])] =
      ((commentary.rep0 ~ strMove.rep0) ~ (result <* escape).? <* commentary.rep0).map {
        case ((coms, sans), res) => (InitialPosition(cleanComments(coms)), sans.toList, res)
      }

  private object MoveParser:

    def rangeToMap(r: Iterable[Char]) = r.zipWithIndex.toMap.view.mapValues(_ + 1).toMap

    val fileMap = rangeToMap('a' to 'h')
    val rankMap = rangeToMap('1' to '8')

    val castleQSide = List("O-O-O", "o-o-o", "0-0-0", "O‑O‑O", "o‑o‑o", "0‑0‑0", "O–O–O", "o–o–o", "0–0–0")
    val qCastle: P[Side] = P.stringIn(castleQSide).as(QueenSide)

    val castleKSide = List("O-O", "o-o", "0-0", "O‑O", "o‑o", "0‑0", "O–O", "o–o", "0–0")

    val kCastle: P[Side] = P.stringIn(castleKSide).as(KingSide)

    val glyph: P[Glyph] =
      mapParser(
        Glyph.MoveAssessment.all
          .sortBy(_.symbol.length)
          .map { g =>
            g.symbol -> g
          }
          .toMap,
        "glyph"
      )

    val glyphs = glyph.rep0.map(gs => Glyphs.fromList(gs))

    val x = P.char('x').?.map(_.isDefined)

    val check = P.char('+').?.map(_.isDefined)

    val checkmate = (P.char('#') | P.string("++")).?.map(_.isDefined)

    val role = mapParserChar(Role.allByPgn, "role")

    val file = mapParserChar(fileMap, "file")

    val rank = mapParserChar(rankMap, "rank")

    val dest: P[Pos] = mapParser(Pos.allKeys, "dest")

    val promotable = Role.allPromotableByPgn.mapKeys(_.toUpper)

    val promotion: P[PromotableRole] = P.char('=').?.with1 *> mapParserChar(promotable, "promotion")

    // e5 or e5
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
      Std(dest = de, role = ro, capture = ca, file = File from fi, rank = Rank from ra)
    }

    // optional e.p.
    val optionalEnPassant = (R.wsp.rep0.soft ~ P.string("e.p.")).void.?

    // d7d5 d7xd5
    val disambiguatedPawn: P[Std] = (((file.? ~ rank.?) ~ x).with1 ~ dest <* optionalEnPassant).map {
      case (((fi, ra), ca), de) =>
        Std(dest = de, role = Pawn, capture = ca, file = File from fi, rank = Rank from ra)
    }

    val suffixes: P0[Suffixes] = (promotion.? ~ checkmate ~ check ~ glyphs).map { case (((p, cm), c), g) =>
      Suffixes(c, cm, p, g)
    }

    val castle: P[San] = (qCastle | kCastle).map(Castle(_))

    val standard: P[San] = P.oneOf(
      (disambiguatedPawn :: pawn :: disambiguated :: ambigous :: drop :: pawnDrop :: Nil).map(_.backtrack)
    )

    val move: P[San] = (castle | standard).withContext("Invalid chess move")
    val moveWithSuffix: P[San] = (move ~ suffixes <* escape)
      .map { case (std, suf) =>
        std withSuffixes suf
      }

    def mapParser[A](pairMap: Map[String, A], name: String): P[A] =
      P.stringIn(pairMap.keySet).map(pairMap.apply) | P.failWith(name + " not found")

    def mapParserChar[A](pairMap: Map[Char, A], name: String): P[A] =
      P.charIn(pairMap.keySet).map(pairMap.apply) | P.failWith(name + " not found")

  private object TagParser:

    val tagName: P[String] = R.alpha.rep.string.withContext("Tag name can only contains alphabet characters")
    val escaped: P[String] = P.char('\\') *> (R.dquote | P.char('\\')).string
    val valueChar: P[String]       = escaped | P.charWhere(_ != '"').string
    val tagValue: P[String]        = valueChar.rep0.map(_.mkString).with1.surroundedBy(R.dquote)
    val tagContent: P[Tag]         = ((tagName <* R.wsp.rep) ~ tagValue).map(p => Tag(p._1, p._2))
    val tag: P[Tag]                = tagContent.between(P.char('['), P.char(']')) <* whitespace.rep0
    val tags: P[NonEmptyList[Tag]] = tag.rep

  private val tagsAndMovesParser: P0[ParsedPgn] =
    ((escape *> TagParser.tags.? <* escape) ~ MovesParser.strMoves.?).map {
      case (optionalTags, optionalMoves) => {
        val preTags = Tags(optionalTags.map(_.toList).getOrElse(Nil))
        optionalMoves match
          case None => ParsedPgn(InitialPosition(Nil), preTags, Sans(Nil))
          case Some((init, sans, resultOption)) => {
            val tags =
              resultOption.filterNot(_ => preTags.exists(_.Result)).foldLeft(preTags)(_ + Tag(_.Result, _))
            ParsedPgn(init, tags, Sans(sans))
          }
      }
    }

  private val pgnParser: P0[ParsedPgn] =
    P.string("\uFEFF").? *> escape *> P.string("[pgn]").? *> tagsAndMovesParser <* P
      .string("[/pgn]")
      .? <* escape

  private def showExpectations(context: String, str: String, error: P.Error): String =
    val lm  = LocationMap(str)
    val idx = error.failedAtOffset
    val caret = lm.toCaret(idx).getOrElse {
      throw new RuntimeException("This is impossible")
    }
    val line         = lm.getLine(caret.line).getOrElse("")
    val errorLine    = line ++ "\n" ++ " ".repeat(caret.col) ++ "^"
    val errorMessage = s"$context: [${caret.line + 1}.${caret.col + 1}]: ${expToString(error.expected.head)}"
    errorMessage ++ "\n\n" ++ errorLine ++ "\n" ++ str

  private def expToString(expectation: Expectation): String =
    expectation match
      case Expectation.OneOfStr(_, strs) =>
        strs match
          case one :: Nil => s"expected: $one"
          case _          => s"expected one of: $strs"
      case Expectation.InRange(_, lower, upper) =>
        if (lower == upper)
          s"expected: $lower"
        else
          s"expected char in range: [$lower, $upper]"
      case Expectation.StartOfString(_)       => "expected start of the file"
      case Expectation.EndOfString(_, length) => s"expected end of file but $length characters remaining"
      case Expectation.Length(_, expected, actual) =>
        s"expected $expected more characters but only $actual remaining"
      case Expectation.ExpectedFailureAt(_, matched) =>
        s"expected failure but the parser matched: $matched"
      case Expectation.Fail(_)                    => "Failed"
      case Expectation.FailWith(_, message)       => message
      case Expectation.WithContext(contextStr, _) => contextStr
