package chess
package format.pgn

import scala.util.parsing.combinator._

// http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
object Parser
    extends scalaz.std.OptionInstances
    with scalaz.syntax.ToTraverseOps {

  def apply(pgn: String): Valid[ParsedPgn] = for {
    splitted ← splitTagAndMoves(pgn)
    (tagStr, moveStr) = splitted
    tags ← TagParser(tagStr)
    parsedMoves ← MovesParser(moveStr)
    (sanStrs, resultOption) = parsedMoves
    tags2 = resultOption.filterNot(_ => tags.exists(_.name == Tag.Result)).fold(tags)(t => tags :+ t)
    sans ← sanStrs.map(MoveParser.apply).sequence
  } yield ParsedPgn(tags2, sans)

  trait Logging { self: Parsers =>
    protected val loggingEnabled = false
    protected def as[T](msg: String)(p: => Parser[T]): Parser[T] =
      if (loggingEnabled) log(p)(msg) else p
  }

  object MovesParser extends RegexParsers with Logging {

    override val whiteSpace = """(\s|\t|\r?\n)+""".r

    def apply(pgn: String): Valid[(List[String], Option[Tag])] =
      parseAll(moves, pgn) match {
        case Success((moves, result), _) => scalaz.Validation.success(moves, result map { r => Tag(_.Result, r) })
        case err                         => "Cannot parse moves: %s\n%s".format(err.toString, pgn).failNel
      }

    def moves: Parser[(List[String], Option[String])] = as("moves") {
      rep(move) ~ (result?) ~ (commentary*) ^^ {
        case sans ~ res ~ _ => sans -> res
      }
    }

    val moveRegex = """(0\-0|0\-0\-0|[QKRBNOoa-h][QKRBNa-h1-8xOo\-=\+\#]{1,6})""".r

    def move: Parser[String] = as("move") {
      ((number | commentary)*) ~> moveRegex <~ (moveExtras*)
    }

    def number: Parser[String] = """[1-9]\d*[\s\.]*""".r

    def moveExtras: Parser[Unit] = as("moveExtras") {
      (annotation | nag | variation | commentary) ^^^ ()
    }

    def annotation: Parser[String] =
      "!" | "?" | "!!" | "!?" | "?!" | "??" | "□"

    def nag: Parser[String] = """\$\d+""".r

    def variation: Parser[List[String]] = as("variation") {
      "(" ~> moves <~ ")" ^^ { case (sans, _) => sans }
    }

    def commentary: Parser[String] = blockCommentary | inlineCommentary

    def blockCommentary: Parser[String] = as("block comment") {
      "{" ~> """[^\}]+""".r <~ "}"
    }

    def inlineCommentary: Parser[String] = as("inline comment") {
      ";" ~> """.+""".r
    }

    val result: Parser[String] = "*" | "1/2-1/2" | "0-1" | "1-0"
  }

  object MoveParser extends RegexParsers with Logging {

    override def skipWhitespace = false

    def apply(str: String): Valid[San] =
      parseAll(move, str) match {
        case Success(san, _) => scalaz.Validation.success(san)
        case err             => "Cannot parse move: %s\n%s".format(err.toString, str).failNel
      }

    def move: Parser[San] = castle | standard

    def castle = (qCastle | kCastle) ~ suffixes ^^ {
      case side ~ suf => Castle(side) withSuffixes suf
    }

    val qCastle: Parser[Side] = ("O-O-O" | "o-o-o" | "0-0-0") ^^^ QueenSide

    val kCastle: Parser[Side] = ("O-O" | "o-o" | "0-0") ^^^ KingSide

    def standard: Parser[Std] = as("standard") {
      (pawn | disambiguated | ambiguous) ~ suffixes ^^ {
        case std ~ suf => std withSuffixes suf
      }
    }

    // e5
    def pawn: Parser[Std] = as("pawn") {
      dest ^^ {
        case de => Std(dest = de, role = Pawn)
      }
    }

    // Bg5
    def ambiguous: Parser[Std] = as("ambiguous") {
      role ~ x ~ dest ^^ {
        case ro ~ ca ~ de => Std(dest = de, role = ro, capture = ca)
      }
    }

    // Bac3 Baxc3 B2c3 B2xc3 Ba2xc3
    def disambiguated: Parser[Std] = as("disambiguated") {
      role ~ opt(file) ~ opt(rank) ~ x ~ dest ^^ {
        case ro ~ fi ~ ra ~ ca ~ de => Std(
          dest = de, role = ro, capture = ca, file = fi, rank = ra)
      }
    }

    def suffixes: Parser[Suffixes] = opt(promotion) ~ check ~ checkmate ^^ {
      case p ~ c ~ cm => Suffixes(c, cm, p)
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
        case (acc, (a, b)) => a.toString ^^^ b | acc
      }
  }

  private object TagParser extends RegexParsers with Logging {

    def apply(pgn: String): Valid[List[Tag]] = parseAll(all, pgn) match {
      case f: Failure       => "Cannot parse tags: %s\n%s".format(f.toString, pgn).failNel
      case Success(sans, _) => scalaz.Validation.success(sans)
      case err              => "Cannot parse tags: %s\n%s".format(err.toString, pgn).failNel
    }

    def all: Parser[List[Tag]] = as("all") {
      tags <~ """(.|\n)*""".r
    }

    def tags: Parser[List[Tag]] = rep(tag)

    def tag: Parser[Tag] = as("tag") {
      tagName ~ tagValue ^^ {
        case name ~ value => Tag(name, value)
      }
    }

    val tagName: Parser[String] = "[" ~> """[a-zA-Z]+""".r

    val tagValue: Parser[String] = "\"" ~> """[^"]+""".r <~ "\"]"
  }

  private def splitTagAndMoves(pgn: String): Valid[(String, String)] =
    pgn.lines.toList.map(_.trim).filter(_.nonEmpty) span { line =>
      ~((line lift 0).map('[' ==))
    } match {
      case (tagLines, moveLines) => success(tagLines.mkString("\n") -> moveLines.mkString("\n"))
    }
}
