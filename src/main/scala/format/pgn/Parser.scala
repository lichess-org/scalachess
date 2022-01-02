package chess
package format.pgn

import chess.variant.Variant

import cats.parse.{ Parser => P, Parser0 => P0, Rfc5234 => R }
import scala.util.parsing.combinator._
import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._
import scala.util.matching.Regex

// http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
object Parser {

  case class StrMove(
      san: String,
      glyphs: Glyphs,
      comments: List[String],
      variations: List[List[StrMove]]
  )

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
        strMoves     = parsedMoves._2
        resultOption = parsedMoves._3
        tags         = resultOption.filterNot(_ => preTags.exists(_.Result)).foldLeft(preTags)(_ + _)
        sans <- objMoves(strMoves, tags.variant | Variant.default)
      } yield ParsedPgn(init, tags, sans)
    } catch {
      case _: StackOverflowError =>
        sys error "### StackOverflowError ### in PGN parser"
    }

  def moves(str: String, variant: Variant): Validated[String, Sans] =
    moves(
      str.split(' ').toList,
      variant
    )
  def moves(strMoves: Iterable[String], variant: Variant): Validated[String, Sans] =
    objMoves(
      strMoves.map { StrMove(_, Glyphs.empty, Nil, Nil) }.to(List),
      variant
    )
  def objMoves(strMoves: List[StrMove], variant: Variant): Validated[String, Sans] =
    strMoves.map { case StrMove(san, glyphs, comments, variations) =>
      (
        MoveParser(san, variant) map { m =>
          m withComments comments withVariations {
            variations
              .map { v =>
                objMoves(v, variant) getOrElse Sans.empty
              }
              .filter(_.value.nonEmpty)
          } mergeGlyphs glyphs
        }
      ): Validated[String, San]
    }.sequence map { Sans.apply }

  trait Logging { self: Parsers =>
    protected val loggingEnabled = false
    protected def as[T](msg: String)(p: => Parser[T]): Parser[T] =
      if (loggingEnabled) log(p)(msg) else p
  }

  object MovesParser extends RegexParsers with Logging {

    override val whiteSpace = """(\s|\t|\r?\n)+""".r

    private def cleanComments(comments: List[String]) = comments.map(_.trim).filter(_.nonEmpty)

    def apply(pgn: String): Validated[String, (InitialPosition, List[StrMove], Option[Tag])] =
      parseAll(strMoves, pgn) match {
        case Success((init, moves, result), _) =>
          valid(
            (
              init,
              moves,
              result map { r =>
                Tag(_.Result, r)
              }
            )
          )
        case err => invalid("Cannot parse moves: %s\n%s".format(err.toString, pgn))
      }

    def strMoves: Parser[(InitialPosition, List[StrMove], Option[String])] =
      as("moves") {
        (commentary *) ~ (strMove *) ~ (result ?) ~ (commentary *) ^^ { case coms ~ sans ~ res ~ _ =>
          (InitialPosition(cleanComments(coms)), sans, res)
        }
      }

    val moveRegex =
      """(?:(?:0\-0(?:\-0|)[\+\#]?)|[PQKRBNOoa-h@][QKRBNa-h1-8xOo\-=\+\#\@]{1,7})[\?!□]{0,2}""".r

    def forbidNullMove: Parser[Unit] =
      as("forbidNullMove") {
        val nullMove = "--" | "Z0" | "null" | "pass" | "@@@@"
        guard(nullMove) ~> err("Lichess does not support null moves") | success(())
      }

    def strMove: Parser[StrMove] =
      as("move") {
        ((number | commentary) *) ~ forbidNullMove ~>
          (moveRegex ~ nagGlyphs ~ rep(commentary) ~ nagGlyphs ~ rep(variation)) <~
          (moveExtras *) ^^ { case san ~ glyphs ~ comments ~ glyphs2 ~ variations =>
            StrMove(san, glyphs merge glyphs2, cleanComments(comments), variations)
          }
      }

    def number: Parser[String] = """[1-9]\d*[\s\.]*""".r

    def moveExtras: Parser[Unit] =
      as("moveExtras") {
        commentary.^^^(())
      }

    def nagGlyphs: Parser[Glyphs] =
      as("nagGlyphs") {
        rep(nag) ^^ { nags =>
          Glyphs fromList nags.flatMap { Glyph.find _ }
        }
      }

    val nagGlyphsRE = Glyph.PositionAssessment.all
      .map(_.symbol)
      .sortBy(-_.length)
      .map(Regex.quote(_))
      .mkString("|")
      .r

    def nag: Parser[String] =
      as("nag") {
        """\$\d+""".r | nagGlyphsRE
      }

    def variation: Parser[List[StrMove]] =
      as("variation") {
        "(" ~> strMoves <~ ")" ^^ { case (_, sms, _) => sms }
      }

    def commentary: Parser[String] = blockCommentary | inlineCommentary

    def blockCommentary: Parser[String] =
      as("block comment") {
        "{" ~> """[^\}]*""".r <~ "}"
      }

    def inlineCommentary: Parser[String] =
      as("inline comment") {
        ";" ~> """.+""".r
      }

    val result: Parser[String] = "*" | "1/2-1/2" | "½-½" | "0-1" | "1-0"
  }

  object MoveParser extends RegexParsers with Logging {

    override def skipWhitespace = false

    private def rangeToMap(r: Iterable[Char]) = r.zipWithIndex.to(Map).view.mapValues(_ + 1)
    private val fileMap                       = rangeToMap('a' to 'h')
    private val rankMap                       = rangeToMap('1' to '8')

    private val MoveR = """^(N|B|R|Q|K|)([a-h]?)([1-8]?)(x?)([a-h][0-9])(=?[NBRQ]?)(\+?)(\#?)$""".r
    private val DropR = """^([NBRQP])@([a-h][1-8])(\+?)(\#?)$""".r

    def apply(str: String, variant: Variant): Validated[String, San] = {
      if (str.length == 2) Pos.fromKey(str).fold(slow(str)) { pos =>
        valid(Std(pos, Pawn))
      }
      else
        str match {
          case "O-O" | "o-o" | "0-0"       => valid(Castle(KingSide))
          case "O-O-O" | "o-o-o" | "0-0-0" => valid(Castle(QueenSide))
          case MoveR(role, file, rank, capture, pos, prom, check, mate) =>
            role.headOption.fold[Option[Role]](Option(Pawn))(variant.rolesByPgn.get) flatMap { role =>
              Pos fromKey pos map { dest =>
                valid(
                  Std(
                    dest = dest,
                    role = role,
                    capture = capture != "",
                    file = if (file == "") None else fileMap get file.head,
                    rank = if (rank == "") None else rankMap get rank.head,
                    promotion = if (prom == "") None else variant.rolesPromotableByPgn get prom.last,
                    metas = Metas(
                      check = check.nonEmpty,
                      checkmate = mate.nonEmpty,
                      comments = Nil,
                      glyphs = Glyphs.empty,
                      variations = Nil
                    )
                  )
                )
              }
            } getOrElse slow(str)
          case DropR(roleS, posS, check, mate) =>
            roleS.headOption flatMap variant.rolesByPgn.get flatMap { role =>
              Pos fromKey posS map { pos =>
                valid(
                  Drop(
                    role = role,
                    pos = pos,
                    metas = Metas(
                      check = check.nonEmpty,
                      checkmate = mate.nonEmpty,
                      comments = Nil,
                      glyphs = Glyphs.empty,
                      variations = Nil
                    )
                  )
                )
              }
            } getOrElse invalid(s"Cannot parse drop: $str")
          case _ => slow(str)
        }
    }

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

    val x         = P.char('x').?.map(_.isDefined)
    val check     = P.char('+').?.map(_.isDefined)
    val checkmate = (P.char('#') | P.string("++")).?.map(_.isDefined)
    val role      = mapParser(Role.allByPgn, "role")

    val file = mapParser(fileMap, "file")
    val rank = mapParser(rankMap, "rank")

    val dest: P[Pos] = mapParser(Pos.allKeys, "dest")

    val promotable = Role.allPromotableByPgn mapKeys (_.toUpper)

    val promotion: P[PromotableRole] = P.char('=') *> mapParser(promotable, "promotion")

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

    val castle: P[San] = ((qCastle | kCastle) ~ suffixes).map { case (side, suf) =>
      Castle(side) withSuffixes suf
    }

    val standard: P[San] =
      ((disambiguatedPawn.backtrack | pawn.backtrack | disambiguated.backtrack | ambigous.backtrack | drop.backtrack | pawnDrop.backtrack) ~ suffixes)
        .map { case (std, suf) =>
          std withSuffixes suf
        }

    val move = castle | standard
    //val move = (pawn ~ suffixes).map { case (std, suf) => std withSuffixes suf }

    def slow(str: String): Validated[String, San] =
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

    val whitespace: P[Unit]     = R.lf | R.wsp
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
            // TODO when error happen at the first charater return empty Tags, because it is feeded with empty string sometime.
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

