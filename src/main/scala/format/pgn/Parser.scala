package chess
package format.pgn

import variant.Variant

import scala.util.parsing.combinator._
import scalaz.Validation.FlatMap._
import scalaz.Validation.{ success => succezz }

// http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
object Parser extends scalaz.syntax.ToTraverseOps {

  def full(pgn: String): Valid[ParsedPgn] = try {
    val preprocessed = pgn.lines.map(_.trim).filter {
      _.headOption != Some('%')
    }.mkString("\n")
      .replace("[pgn]", "")
      .replace("[/pgn]", "")
    for {
      splitted ← splitTagAndMoves(preprocessed)
      (tagStr, moveStr) = splitted
      tags ← TagParser(tagStr)
      parsedMoves ← MovesParser(moveStr)
      (sanStrs, resultOption) = parsedMoves
      tags2 = resultOption.filterNot(_ => tags.exists(_.name == Tag.Result)).fold(tags)(t => tags :+ t)
      sans ← moves(sanStrs, getVariantFromTags(tags2))
    } yield ParsedPgn(tags2, sans)
  }
  catch {
    case e: StackOverflowError =>
      println(pgn)
      sys error s"### StackOverflowError ### in PGN parser"
  }

  def getVariantFromTags(tags: List[Tag]): Variant = {
    val variant = tags.find(_.name == Tag.Variant)

    variant flatMap (tag => Variant.byName(tag.value)) getOrElse (Variant.default)
  }

  def moves(str: String, variant: Variant): Valid[List[San]] = moves(str.split(' ').toList, variant)
  def moves(strs: List[String], variant: Variant): Valid[List[San]] =
    strs.map(str => MoveParser.apply(str, variant)).sequence

  trait Logging { self: Parsers =>
    protected val loggingEnabled = false
    protected def as[T](msg: String)(p: => Parser[T]): Parser[T] =
      if (loggingEnabled) log(p)(msg) else p
  }

  object MovesParser extends RegexParsers with Logging {

    override val whiteSpace = """(\s|\t|\r?\n)+""".r

    def apply(pgn: String): Valid[(List[String], Option[Tag])] =
      parseAll(moves, pgn) match {
        case Success((moves, result), _) => succezz(moves, result map { r => Tag(_.Result, r) })
        case err                         => "Cannot parse moves: %s\n%s".format(err.toString, pgn).failureNel
      }

    def moves: Parser[(List[String], Option[String])] = as("moves") {
      rep(move) ~ (result?) ~ (commentary*) ^^ {
        case sans ~ res ~ _ => sans -> res
      }
    }

    val moveRegex = """(0\-0\-0|0\-0|[QKRBNOoa-h][QKRBNa-h1-8xOo\-=\+\#]{1,6})""".r

    def move: Parser[String] = as("move") {
      ((number | commentary)*) ~> moveRegex <~ (moveExtras*)
    }

    def number: Parser[String] = """[1-9]\d*[\s\.]*""".r

    def moveExtras: Parser[Unit] = as("moveExtras") {
      (annotation | nag | variation | commentary).^^^(())
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

    private def rangeToMap(r: Iterable[Char]) = r.zipWithIndex.toMap mapValues (_ + 1)
    val fileMap = rangeToMap('a' to 'h')
    val rankMap = rangeToMap('1' to '8')

    private val Move = """^(N|B|R|Q|K|)([a-h]?)([1-8]?)(x?)([a-h][0-9])(=?[NBRQ]?)(\+?)(\#?)$""".r

    def apply(str: String, variant: Variant): Valid[San] = {
      if (str.size == 2) Pos.posAt(str).fold(slow(str)) { pos => succezz(Std(pos, Pawn)) }
      else str match {
        case "O-O" | "o-o" | "0-0"       => succezz(Castle(KingSide))
        case "O-O-O" | "o-o-o" | "0-0-0" => succezz(Castle(QueenSide))
        case Move(role, file, rank, capture, pos, prom, check, mate) =>
          role.headOption.fold[Option[Role]](Some(Pawn))(variant.rolesByPgn.get) flatMap { role =>
            Pos posAt pos map { dest =>
              succezz(Std(
                dest = dest,
                role = role,
                capture = capture != "",
                check = check != "",
                checkmate = mate != "",
                file = if (file == "") None else fileMap get file.head,
                rank = if (rank == "") None else rankMap get rank.head,
                promotion = if (prom == "") None else variant.rolesPromotableByPgn get prom.last))
            }
          } getOrElse slow(str)
        case _ => slow(str)
      }
    }

    private def slow(str: String): Valid[San] =
      parseAll(move, str) match {
        case Success(san, _) => succezz(san)
        case err             => "Cannot parse move: %s\n%s".format(err.toString, str).failureNel
      }

    def move: Parser[San] = castle | standard

    def castle = (qCastle | kCastle) ~ suffixes ^^ {
      case side ~ suf => Castle(side) withSuffixes suf
    }

    val qCastle: Parser[Side] = ("O-O-O" | "o-o-o" | "0-0-0") ^^^ QueenSide

    val kCastle: Parser[Side] = ("O-O" | "o-o" | "0-0") ^^^ KingSide

    def standard: Parser[Std] = as("standard") {
      (disambiguatedPawn | pawn | disambiguated | ambiguous) ~ suffixes ^^ {
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

    // d7d5
    def disambiguatedPawn: Parser[Std] = as("disambiguated") {
      opt(file) ~ opt(rank) ~ x ~ dest ^^ {
        case fi ~ ra ~ ca ~ de => Std(
          dest = de, role = Pawn, capture = ca, file = fi, rank = ra)
      }
    }

    def suffixes: Parser[Suffixes] = opt(promotion) ~ checkmate ~ check ^^ {
      case p ~ cm ~ c => Suffixes(c, cm, p)
    }

    val x = exists("x")

    val check = exists("+")

    val checkmate = ("#" | "++") ^^^ true | success(false)

    val role = mapParser(Role.allByPgn, "role") | success(Pawn)

    val file = mapParser(fileMap, "file")

    val rank = mapParser(rankMap, "rank")

    val promotion = ("="?) ~> mapParser(promotable, "promotion")

    val promotable = Role.allPromotableByPgn mapKeys (_.toUpper)

    val dest = mapParser(Pos.allKeys, "dest")

    def exists(c: String): Parser[Boolean] = c ^^^ true | success(false)

    def mapParser[A, B](map: Map[A, B], name: String): Parser[B] =
      map.foldLeft(failure(name + " not found"): Parser[B]) {
        case (acc, (a, b)) => a.toString ^^^ b | acc
      }
  }

  private object TagParser extends RegexParsers with Logging {

    def apply(pgn: String): Valid[List[Tag]] = parseAll(all, pgn) match {
      case f: Failure       => "Cannot parse tags: %s\n%s".format(f.toString, pgn).failureNel
      case Success(sans, _) => succezz(sans)
      case err              => "Cannot parse tags: %s\n%s".format(err.toString, pgn).failureNel
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

    val tagValue: Parser[String] = """[^\]]+""".r <~ "]" ^^ {
      _.replace("\"", "")
    }
  }

  private def splitTagAndMoves(pgn: String): Valid[(String, String)] =
    pgn.lines.toList.map(_.trim).filter(_.nonEmpty) span { line =>
      ~((line lift 0).map('[' ==))
    } match {
      case (tagLines, moveLines) => success(tagLines.mkString("\n") -> moveLines.mkString("\n"))
    }
}
