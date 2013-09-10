package chess
package format.pgn

import scala.util.parsing.combinator._

object Parser {

  def apply(pgn: String): Valid[ParsedPgn] = for {
    splitted ← splitSanAndMoves(sanitize(pgn))
    (tagStr, moveStr) = splitted
    tags ← TagParser(tagStr)
    sans ← MoveParser(moveStr)
  } yield ParsedPgn(tags, sans)

  private val SanitizeRegex = """(\$\d+)|(\{[^\}]+\})""".r
  private def sanitize(pgn: String): String = 
    SanitizeRegex.replaceAllIn(pgn.lines.map(_.trim).filterNot(_.isEmpty) mkString "\n", "")

  private def splitSanAndMoves(pgn: String): Valid[(String, String)] =
    pgn.lines.toList span { line ⇒
      ~((line lift 0).map('[' ==))
    } match {
      case (tagLines, moveLines) ⇒ success(tagLines.mkString("\n") -> moveLines.mkString(" ").trim)
    }

  object TagParser extends RegexParsers {

    def apply(pgn: String): Valid[List[Tag]] = parseAll(all, pgn) match {
      case f: Failure       ⇒ "Cannot parse tags: %s\n%s".format(f.toString, pgn).failNel
      case Success(sans, _) ⇒ scalaz.Scalaz.success(sans)
      case err              ⇒ "Cannot parse tags: %s\n%s".format(err.toString, pgn).failNel
    }

    def all: Parser[List[Tag]] = tags <~ """(.|\n)*""".r 

    def tags: Parser[List[Tag]] = rep(tag)

    def tag: Parser[Tag] = tagName ~ tagValue ^^ {
      case name ~ value ⇒ Tag(name, value)
    }

    val tagName: Parser[String] = "[" ~> """[a-zA-Z]+""".r

    val tagValue: Parser[String] = "\"" ~> """[^"]+""".r <~ "\"]"
  }

  object MoveParser extends RegexParsers {

    val CRLF = "\r\n" | "\n"
    val space = """\s+""".r

    override val whiteSpace = "".r

    def apply(pgn: String): Valid[List[San]] =
      parseAll(moves, (pgn.lines mkString " ")) match {
        case Success(sans, _) ⇒ scalaz.Scalaz.success(sans)
        case err              ⇒ "Cannot parse moves: %s\n%s".format(err.toString, pgn).failNel
      }

    def moves: Parser[List[San]] = repsep(move, space) <~ (result?)

    val result: Parser[String] = space ~> ("*" | "1/2-1/2" | "0-1" | "1-0")

    def move: Parser[San] =
      (number?) ~> (castle | standard) <~ (nag?) <~ (comment?)

    def nag: Parser[String] = """[^\s]*""".r

    def comment: Parser[String] = space ~> "{" ~> """[^\}]+""".r <~ "}"

    def castle = (qCastle | kCastle) ~ suffixes ^^ {
      case side ~ suf ⇒ Castle(side) withSuffixes suf
    }

    val qCastle: Parser[Side] = "O-O-O" ^^^ QueenSide

    val kCastle: Parser[Side] = "O-O" ^^^ KingSide

    def standard: Parser[Std] = (complete | simple | disambiguated) ~ suffixes ^^ {
      case std ~ suf ⇒ std withSuffixes suf
    }

    val number: Parser[String] = """\d+\.+""".r <~ maybeSpace

    val maybeSpace = space?

    def simple: Parser[Std] = role ~ x ~ dest ^^ {
      case ro ~ ca ~ de ⇒ Std(dest = de, role = ro, capture = ca)
    }

    def disambiguated: Parser[Std] = role ~ opt(file) ~ opt(rank) ~ x ~ dest ^^ {
      case ro ~ fi ~ ra ~ ca ~ de ⇒ Std(
        dest = de, role = ro, capture = ca, file = fi, rank = ra)
    }

    def complete: Parser[Std] = role ~ file ~ rank ~ x ~ dest ^^ {
      case ro ~ fi ~ ra ~ ca ~ de ⇒ Std(
        dest = de, role = ro, capture = ca, file = fi.some, rank = ra.some)
    }

    def suffixes: Parser[Suffixes] = opt(promotion) ~ check ~ checkmate ^^ {
      case p ~ c ~ cm ⇒ Suffixes(c, cm, p)
    }

    val x = exists("x")

    val check = exists("+")

    val checkmate = exists("#")

    val role = mapParser(Role.allByPgn, "role") | success(Pawn)

    val file = mapParser(rangeToMap('a' to 'h'), "file")

    val rank = mapParser(rangeToMap('1' to '8'), "rank")

    val promotion = "=" ~> mapParser(promotable, "promotion")

    val promotable = Role.allPromotableByPgn mapKeys (_.toUpper)

    val dest = mapParser(Pos.allKeys, "dest")

    def exists(c: String): Parser[Boolean] = c ^^^ true | success(false)

    def rangeToMap(r: Iterable[Char]) = r.zipWithIndex.toMap mapValues (_ + 1)

    def mapParser[A, B](map: Map[A, B], name: String): Parser[B] =
      map.foldLeft(failure(name + " not found"): Parser[B]) {
        case (acc, (a, b)) ⇒ a.toString ^^^ b | acc
      }
  }
}
