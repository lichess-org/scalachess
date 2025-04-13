package chess
package format.pgn

import cats.parse.Parser.Expectation
import cats.parse.{ LocationMap, Numbers as N, Parser as P, Parser0 as P0, Rfc5234 as R }
import cats.syntax.all.*

// http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
object Parser:

  // https://unicode-explorer.com/c/00A0
  private val nbsp       = P.char('\u00A0')
  private val whitespace = R.cr | R.lf | R.wsp | nbsp | P.char('\uFEFF')
  val pgnComment         = P.caret.filter(_.col == 0) *> P.char('%') *> P.until(P.char('\n')).void
  // pgnComment with % or whitespaces
  private val escape = pgnComment.? *> whitespace.rep0.?

  def full(pgn: PgnStr): Either[ErrorStr, ParsedPgn] =
    pgnParser.parse(pgn.value, "Cannot parse pgn")

  /**
    * Parse the mainline of a PGN file ignoring variations
    *
    */
  def mainline(pgn: PgnStr): Either[ErrorStr, ParsedMainline] =
    pgnMainlineParser.parse(pgn.value, "Cannot parse pgn")

  def moves(strMoves: Iterable[SanStr]): Either[ErrorStr, Sans] =
    strMoves.toList.traverse(sanOnly).map(Sans(_))

  def moves(str: PgnMovesStr): Either[ErrorStr, Option[ParsedPgnTree]] =
    moveParser.rep0
      .parse(str.value, "Cannot parse moves")
      .map(Tree.build)

  def move(str: SanStr): Either[ErrorStr, ParsedPgnTree] =
    moveParser.parse(str.value, "Cannot parse move")

  def sanOnly(str: SanStr): Either[ErrorStr, San] =
    sanOnly.parse(str.value, "Cannot parse move")

  private val blockComment  = P.until0(P.char('}')).with1.between(P.char('{'), P.char('}')).map(Comment(_))
  private val inlineComment = P.char(';') *> P.until(R.lf).map(Comment(_))
  private val comment       = (blockComment | inlineComment).withContext("Invalid comment") <* escape

  private def mapResult(result: String): String = Outcome.fromResult(result).fold(result)(_.toString)

  private val result: P[String] = P.stringIn(Outcome.knownResultStrings).map(mapResult)

  private val nagGlyphsRE = P.stringIn(Glyph.PositionAssessment.all.map(_.symbol))

  private val nag = (P.char('$') ~ R.digit.rep).string | nagGlyphsRE

  private val nagGlyphs: P0[Glyphs] =
    (nag <* escape).rep0.map(nags => Glyphs.fromList(nags.flatMap(Glyph.find(_))))

  private val moveExtras = comment.void

  private val positiveIntString: P[String] = (N.nonZeroDigit ~ N.digits0).string

  // '. ' or '... ' or '. ... '
  private val numberSuffix = (P.char('.') | whitespace).rep0.void

  // 10. or 10... but not 0 or 1-0 or 1/2
  private val number = (positiveIntString <* !P.charIn('‑', '–', '-', '/', '½') ~ numberSuffix).string

  private val forbidNullMove = P
    .stringIn(List("--", "Z0", "null", "pass", "@@@@"))
    .?
    .flatMap(o => o.fold(P.unit)(_ => P.failWith("Null moves are not supported").void))

  extension (p: P0[Any])
    private def endWith(p1: P[Any]): P[String] = p.with1 *> (p1.string | (P.until(p1) <* p1))

  private val preMoveEscape    = ((number.backtrack | comment).rep0 ~ forbidNullMove).void
  private val moveAndMetas     = SanParser.san ~ SanParser.metas
  private val postMoveEscape   = moveExtras.rep0.void <* escape
  private val escapeVariations = (P.char('(').endWith(P.char(')')).void ~ escape).void.rep0.void

  private val sanOnly: P[San] =
    preMoveEscape.with1 *> SanParser.san <* (SanParser.metas.void ~ escapeVariations ~ postMoveEscape.void)

  private val sanAndMetasOnly: P[SanWithMetas] =
    P.recursive[SanWithMetas] { recuse =>

      val variation: P[Unit] =
        (P.char('(') *> comment.rep0.surroundedBy(escape) *> recuse.rep0 *> P.char(')') *> escape).void

      (preMoveEscape.with1 *> (moveAndMetas <* variation.rep0) <* postMoveEscape)
        .map(SanWithMetas.apply)
    }

  private val moveParser: P[Node[PgnNodeData]] =
    P.recursive[Node[PgnNodeData]] { recuse =>
      // TODO: if a variation only contains comments, we ignore it
      // Will fix it after support null move
      val variation: P[Option[Variation[PgnNodeData]]] =
        (P.char('(') *> comment.rep0.surroundedBy(escape) ~ recuse.rep0 <* (P.char(')') ~ escape))
          .map((comments, sans) =>
            sans match
              case Nil => None
              case x :: xs =>
                Variation(x.value.copy(variationComments = comments.cleanUp), Tree.build(xs)).some
          )

      preMoveEscape.with1 *> ((moveAndMetas ~ variation.rep0) <* postMoveEscape).map:
        case ((san, metas), vs) =>
          val data = PgnNodeData(san, metas, Nil)
          Node(data, None, vs.flatten)
    }

  private val fullMovesParser: P0[(InitialComments, Option[ParsedPgnTree], Option[String])] =
    ((comment.rep0 ~ moveParser.rep0) ~ (result <* escape).? <* comment.rep0).map:
      case ((comments, sans), res) =>
        (InitialComments(comments.cleanUp), Tree.build(sans), res)

  private val fullSanAndMetasParser: P0[(InitialComments, List[SanWithMetas], Option[String])] =
    ((comment.rep0 ~ sanAndMetasOnly.rep0) ~ (result <* escape).? <* comment.rep0).map:
      case ((comments, sans), res) =>
        (InitialComments(comments.cleanUp), sans, res)

  private object SanParser:

    val fileMap = File.all.mapBy(_.char)
    val rankMap = Rank.all.mapBy(_.char)

    val castleQSide = List("O-O-O", "o-o-o", "0-0-0", "O‑O‑O", "o‑o‑o", "0‑0‑0", "O–O–O", "o–o–o", "0–0–0")
    val qCastle: P[Side] = P.stringIn(castleQSide).as(QueenSide)

    val castleKSide      = List("O-O", "o-o", "0-0", "O‑O", "o‑o", "0‑0", "O–O", "o–o", "0–0")
    val kCastle: P[Side] = P.stringIn(castleKSide).as(KingSide)

    val glyph: P[Glyph] = mapParser(Glyph.MoveAssessment.all.mapBy(_.symbol), "glyph")
    val glyphs          = glyph.rep0.map(Glyphs.fromList)

    val capture = P.char('x').?.map(_.isDefined)

    val check = P.char('+').?.map(o => Check(o.isDefined))

    val checkmate = (P.char('#') | P.string("++")).?.map(_.isDefined)

    val role = mapParserChar(Role.allByPgn, "role")

    val file: P[File] = mapParserChar(fileMap, "file")

    val rank: P[Rank] = mapParserChar(rankMap, "rank")

    val dest: P[Square] = mapParser(Square.allKeys, "dest")

    val promotable = Role.allPromotableByPgn.mapKeys(_.toUpper)

    val promotion: P[PromotableRole] = P.char('=').?.with1 *> mapParserChar(promotable, "promotion")

    // e5 or e5
    val stdPawn: P[Std] = dest.map(Std(_, Pawn))

    // Bg5
    val ambigous: P[Std] = (role ~ capture ~ dest).map:
      case ((ro, ca), de) =>
        Std(dest = de, role = ro, capture = ca)

    // B@g5
    val drop: P[Drop] = ((role <* P.char('@')) ~ dest).map(Drop(_, _))

    val pawnDrop: P[Drop] = (P.char('@') *> dest).map(Drop(Pawn, _))

    // optional e.p.
    val optionalEnPassant = (R.wsp.rep0.soft ~ P.stringIn(List("e.p.", "ep"))).void.?

    // d7d5 d7xd5
    val disambiguatedPawn: P[Std] = (((file.? ~ rank.?) ~ capture).with1 ~ dest <* optionalEnPassant).map:
      case (((file, rank), capture), dest) =>
        Std(dest, Pawn, capture, file, rank)

    // only pawn can promote
    val pawn: P[Std] = ((disambiguatedPawn.backtrack | stdPawn) ~ promotion.?).map: (pawn, promo) =>
      pawn.copy(promotion = promo)

    // Bac3 Baxc3 B2c3 B2xc3 Ba2xc3
    val disambiguated: P[Std] = (role ~ file.? ~ rank.? ~ capture ~ dest).map:
      case ((((role, file), rank), capture), dest) =>
        Std(dest, role, capture, file, rank)

    val metas: P0[Metas] =
      (checkmate, check, (glyphs <* escape), nagGlyphs, comment.rep0, nagGlyphs)
        .mapN: (checkmate, check, glyphs1, glyphs2, comments, glyphs3) =>
          val glyphs = glyphs1.merge(glyphs2.merge(glyphs3))
          Metas(check, checkmate, comments.cleanUp, glyphs)

    val castle: P[San] = (qCastle | kCastle).map(Castle(_))

    val standard: P[San] =
      P.oneOf((pawn :: disambiguated :: ambigous :: drop :: pawnDrop :: Nil).map(_.backtrack))

    val san: P[San] = (castle | standard).withContext("Invalid chess move")

    def mapParser[A](pairMap: Map[String, A], name: String): P[A] =
      P.stringIn(pairMap.keySet).map(pairMap.apply) | P.failWith(name + " not found")

    def mapParserChar[A](pairMap: Map[Char, A], name: String): P[A] =
      P.charIn(pairMap.keySet).map(pairMap.apply) | P.failWith(name + " not found")

  private object TagParser:

    val tagName: P[String] = R.alpha.rep.string.withContext("Tag name can only contains alphabet characters")
    val escaped: P[String] = P.char('\\') *> (R.dquote | P.char('\\')).string
    val valueChar: P[String] = escaped | P.charWhere(_ != '"').string
    val tagValue: P[String]  = valueChar.rep0.map(_.mkString).with1.surroundedBy(R.dquote)
    val tagContent: P[Tag]   = ((tagName <* R.wsp.rep) ~ tagValue).map(Tag(_, _))
    val tag: P[Tag]          = tagContent.between(P.char('['), P.char(']')) <* whitespace.rep0
    val tags: P0[List[Tag]]  = tag.rep0

  private val tagsAndMovesParser: P0[ParsedPgn] =
    (TagParser.tags.surroundedBy(escape) ~ fullMovesParser.?).map: (optionalTags, optionalMoves) =>
      val preTags = Tags.sanitize(optionalTags)
      optionalMoves match
        case None => ParsedPgn(InitialComments.empty, preTags, None)
        case Some((init, tree, result)) =>
          val tags = result.filterNot(_ => preTags.exists(_.Result)).foldLeft(preTags)(_ + Tag(_.Result, _))
          ParsedPgn(init, tags, tree)

  private val mainlineParser: P0[ParsedMainline] =
    (TagParser.tags.surroundedBy(escape) ~ fullSanAndMetasParser.?).map: (optionalTags, optionalMoves) =>
      val preTags = Tags.sanitize(optionalTags)
      optionalMoves match
        case None => ParsedMainline(preTags, Nil)
        case Some((_, sans, result)) =>
          val tags = result.filterNot(_ => preTags.exists(_.Result)).foldLeft(preTags)(_ + Tag(_.Result, _))
          ParsedMainline(tags, sans)

  def escapePgnTag[A](p: P0[A]): P0[A] =
    escape *> P.string("[pgn]").? *> p <* P.string("[/pgn]").? <* escape

  private val pgnParser: P0[ParsedPgn]              = escapePgnTag(tagsAndMovesParser)
  private val pgnMainlineParser: P0[ParsedMainline] = escapePgnTag(mainlineParser)

  extension [A](p: P0[A])
    def parse(str: String, context: String): Either[ErrorStr, A] =
      p.parse(str).bimap(err => showExpectations(context, str, err), _._2)

  private def showExpectations(context: String, str: String, error: P.Error): ErrorStr =
    val lm  = LocationMap(str)
    val idx = error.failedAtOffset
    val caret = lm
      .toCaret(idx)
      .getOrElse(throw RuntimeException("This is impossible"))
    val line         = lm.getLine(caret.line).getOrElse("")
    val errorLine    = line ++ "\n" ++ " ".*(caret.col) ++ "^"
    val errorMessage = s"$context: [${caret.line + 1}.${caret.col + 1}]: ${expToString(error.expected.head)}"
    ErrorStr(s"$errorMessage\n\n$errorLine\n$str")

  private def expToString(expectation: Expectation): String =
    expectation match
      case Expectation.OneOfStr(_, strs) =>
        strs match
          case one :: Nil => s"expected: $one"
          case _          => s"expected one of: $strs"
      case Expectation.InRange(_, lower, upper) =>
        if lower == upper then s"expected: $lower"
        else s"expected char in range: [$lower, $upper]"
      case Expectation.StartOfString(_)       => "expected start of the file"
      case Expectation.EndOfString(_, length) => s"expected end of file but $length characters remaining"
      case Expectation.Length(_, expected, actual) =>
        s"expected $expected more characters but only $actual remaining"
      case Expectation.ExpectedFailureAt(_, matched) =>
        s"expected failure but the parser matched: $matched"
      case Expectation.Fail(_)                    => "Failed"
      case Expectation.FailWith(_, message)       => message
      case Expectation.WithContext(contextStr, _) => contextStr
