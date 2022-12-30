package chess
package perft

import cats.syntax.all.*
import cats.parse.{ LocationMap, Numbers as N, Parser as P, Parser0 as P0, Rfc5234 as R }

import chess.format.EpdFen

/**
  * Perft parser specification
  *
  * perft -> id "\n" epd "\n" case* "\n"
  * id -> "id " STRING
  * epd -> "epd " EpdFen
  * case -> "perft " INT INT "\n"
  *
  * -- only support comment start at the begining of the ling
  * comment = "#" STRING "\n"
  *
  */
object Parser:
  def parse: String => Either[P.Error, (String, List[Perft1])] = {
    // println(id.parse("id aligng\n"))
    println(((ignored.rep0 *> id) ~ epd).parse("""#
# id align-ep
id align-ep
epd 8/8/8/1k6/3Pp3/8/8/4KQ2 b - d3
"""))
    perfts.parse
  }

  private val whitespace = R.cr | R.lf | R.wsp
  private val blank = P.until(!whitespace)
  private val nonNegative = N.nonNegativeIntString

  private val comment = (P.caret.filter(_.col == 0) *> P.char('#')).endWith(R.lf)
  private val ignored = (comment | blank).void

  private val id: P[String] = "id".prefix
  private val epd: P[EpdFen] = "epd".prefix.map(EpdFen.clean)
  private val testCase: P[TestCase] = ((nonNegative.map(_.toInt) <* P.char(' ')) ~ nonNegative.map(_.toLong)).map(TestCase.apply)
  private val oneTestCase: P[TestCase] = P.string("perft ") *> testCase <* R.lf.?
  private val cases: P[List[TestCase]] = oneTestCase.rep.map(_.toList) <* (ignored.rep | R.lf.rep0)
  private val perft: P[Perft1] = (id, epd, cases).mapN(Perft1.apply) <* R.lf.?
  // private val perfts = perft.rep.map(_.toList)
  private val perfts = ignored.rep0 *> perft.rep.map(_.toList)


  extension (p: P0[Any])
    private def endWith(p1: P[Any]): P[String] = p.with1 *> (p1.string | (P.until(p1) <* p1))

  extension (str: String)
    private def prefix: P[String] = P.string(s"$str ").endWith(R.lf)
