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

  final val pgnContext = "Error parsing PGN"

  def full(pgn: PgnStr): Either[ErrorStr, ParsedPgn] =
    pgnParser.parse(pgn.value, pgnContext)

  /**
    * Parse the mainline with san moves and ignoring variations
    */
  def mainline(pgn: PgnStr): Either[ErrorStr, ParsedMainline[San]] =
    pgnSansParser.parse(pgn.value, pgnContext)

  /**
    * Parse the mainline of a PGN file ignoring variations
    * similar to the [[mainline]] but with metas
    */
  def mainlineWithMetas(pgn: PgnStr): Either[ErrorStr, ParsedMainline[SanWithMetas]] =
    pgnMainlineParser.parse(pgn.value, pgnContext)

  def tags(pgn: PgnStr): Either[ErrorStr, Tags] =
    tagsParser.parse(pgn.value, "Error parsing tags").map(Tags(_))

  def moves(strMoves: Iterable[SanStr]): Either[ErrorStr, Sans] =
    strMoves.toList.traverse(san).map(Sans(_))

  def moves(str: PgnMovesStr): Either[ErrorStr, Option[ParsedPgnTree]] =
    moveParser.rep0
      .parse(str.value, "Error parsing moves")
      .map(Tree.build)

  def move(str: SanStr): Either[ErrorStr, ParsedPgnTree] =
    moveParser.parse(str.value, "Error parsing move")

  def san(str: SanStr): Either[ErrorStr, San] =
    simpleSan.parse(str.value, "Error parsing move")

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

  private val preMoveEscape  = ((number.backtrack | comment).rep0 ~ forbidNullMove).void
  private val moveAndMetas   = SanParser.san ~ SanParser.metas
  private val postMoveEscape = moveExtras.rep0.void <* escape

  private val simpleSan: P[San] =
    preMoveEscape.with1 *> SanParser.san

  private val sanOnly: P[San] =
    escapeVariations(SanParser.san <* SanParser.metas)

  private val sanAndMetasOnly: P[SanWithMetas] =
    escapeVariations(moveAndMetas.map(SanWithMetas.apply))

  private def escapeVariations[A](p: P[A]): P[A] =
    P.recursive[A] { recurse =>
      val variation: P[Unit] =
        (P.char('(') *> comment.rep0.surroundedBy(escape) *> recurse.rep0 *> P.char(')') *> escape).void
      (preMoveEscape.with1 *> (p <* variation.rep0) <* postMoveEscape)
    }

  private val moveParser: P[Node[PgnNodeData]] =
    P.recursive[Node[PgnNodeData]] { recurse =>
      // TODO: if a variation only contains comments, we ignore it
      // Will fix it after support null move
      val variation: P[Option[Variation[PgnNodeData]]] =
        (P.char('(') *> comment.rep0.surroundedBy(escape) ~ recurse.rep0 <* (P.char(')') ~ escape))
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

  private inline def fullBody[A](p: P[A]): P0[(InitialComments, List[A], Option[String])] =
    ((comment.rep0 ~ p.rep0) ~ (result <* escape).? <* comment.rep0).map:
      case ((comments, xs), result) => (InitialComments(comments.cleanUp), xs, result)

  private val fullMovesParser = fullBody(moveParser)

  private object SanParser:

    val fileMap = File.all.mapBy(_.char)
    val rankMap = Rank.all.mapBy(_.char)

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

    val standard: P[Std] =
      P.oneOf:
        (pawn :: disambiguated :: ambigous :: Nil).map:
          _.backtrack.withString.map((san, raw) => san.copy(rawString = raw.some))

    val castleQSide = List("O-O-O", "o-o-o", "0-0-0", "O‑O‑O", "o‑o‑o", "0‑0‑0", "O–O–O", "o–o–o", "0–0–0")
    val qCastle: P[Side] = P.stringIn(castleQSide).as(QueenSide)

    val castleKSide      = List("O-O", "o-o", "0-0", "O‑O", "o‑o", "0‑0", "O–O", "o–o", "0–0")
    val kCastle: P[Side] = P.stringIn(castleKSide).as(KingSide)

    val castle: P[San] = (qCastle | kCastle).withString.map((side, raw) => Castle(side, raw.some))

    // B@g5
    val pieceDrop: P[Drop] = ((role <* P.char('@')) ~ dest).map(Drop(_, _))

    val pawnDrop: P[Drop] = (P.char('@') *> dest).map(Drop(Pawn, _))

    val drop: P[Drop] =
      P.oneOf:
        (pieceDrop :: pawnDrop :: Nil).map:
          _.backtrack.withString.map((san, raw) => san.copy(rawString = raw.some))

    val san: P[San] = (castle | standard | drop).withContext("Invalid chess move")

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
    (TagParser.tags.surroundedBy(escape) ~ fullMovesParser.?)
      .map: (optionalTags, optionalMoves) =>
        val preTags = Tags.sanitize(optionalTags)
        optionalMoves match
          case None => ParsedPgn(InitialComments.empty, preTags, None)
          case Some((init, nodes, result)) =>
            ParsedPgn(init, updateTagsWithResult(preTags, result), Tree.build(nodes))

  private inline def tagsAndMainlineParser[A](
      p: P0[(InitialComments, List[A], Option[String])]
  ): P0[ParsedMainline[A]] =
    (TagParser.tags.surroundedBy(escape) ~ p.?).map: (optionalTags, optionalMoves) =>
      val preTags = Tags.sanitize(optionalTags)
      optionalMoves match
        case None => ParsedMainline(InitialComments.empty, preTags, Nil)
        case Some((init, sans, result)) =>
          ParsedMainline(init, updateTagsWithResult(preTags, result), sans)

  private inline def updateTagsWithResult(tags: Tags, result: Option[String]): Tags =
    result.filterNot(_ => tags.exists(_.Result)).foldLeft(tags)(_ + Tag(_.Result, _))

  private inline def escapePgnTag[A](p: P0[A]): P0[A] =
    escape *> P.string("[pgn]").? *> p <* P.string("[/pgn]").? <* escape

  private val tagsParser               = TagParser.tags.surroundedBy(escape)
  private val pgnParser: P0[ParsedPgn] = escapePgnTag(tagsAndMovesParser)

  private val pgnMainlineParser: P0[ParsedMainline[SanWithMetas]] =
    escapePgnTag(tagsAndMainlineParser(fullBody(sanAndMetasOnly)))

  private val pgnSansParser: P0[ParsedMainline[San]] =
    escapePgnTag(tagsAndMainlineParser(fullBody(sanOnly)))

  extension [A](p: P0[A])
    private inline def parse(str: String, context: String): Either[ErrorStr, A] =
      p.parse(str).bimap(showExpectations(context, str), _._2)

  private def showExpectations(context: String, str: String)(error: P.Error): ErrorStr =
    val lm    = LocationMap(str)
    val idx   = error.failedAtOffset
    val caret = lm.toCaret(idx).getOrElse(throw RuntimeException("This is impossible"))
    ErrorStr:
      s"$context: ${expToString(error.expected.head)} at line ${caret.line + 1}, column ${caret.col + 1}"

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
