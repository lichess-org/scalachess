package chess
package perft

import cats.parse.{ DefaultParser0, Numbers as N, Parser as P, Parser0 as P0, Rfc5234 as R }
import cats.syntax.all.*
import chess.format.FullFen

/**
  * Perft parser specification
| *
  * perfts = comment* perft* comment* "\n"*
  *
  * perft -> id "\n" epd "\n" case* "\n"
  * id -> "id " STRING
  * epd -> "epd " FullFen
  * case -> "perft " INT LONG "\n"
  *
  * -- only support comment at the begining of the line
  * comment = "#" STRING "\n"
  *
  */

object Parser extends DefaultParser0[List[Perft]]:

  private val whitespace = R.cr | R.lf | R.wsp
  private val blank = P.until(!whitespace)
  private val nonNegative = N.nonNegativeIntString

  private val comment = (P.caret.filter(_.col == 0) *> P.char('#')).endWith(R.lf)
  private val ignored = (comment | blank).void

  private val id: P[String] = "id".prefix
  private val epd: P[FullFen] = "epd".prefix.map(FullFen.clean)
  private val testCase: P[TestCase] =
    ((nonNegative.map(_.toInt) <* P.char(' ')) ~ nonNegative.map(_.toLong)).map(TestCase.apply)
  private val oneTestCase: P[TestCase] = P.string("perft ") *> testCase <* R.lf.?
  private val cases: P[List[TestCase]] = oneTestCase.rep.map(_.toList) <* (ignored.rep | R.lf.rep0)
  private val perft: P[Perft] = (id, epd, cases).mapN(Perft.apply) <* R.lf.?
  def parser0: P0[List[Perft]] = ignored.rep0 *> perft.rep.map(_.toList)

  extension (p: P0[Any])
    private def endWith(p1: P[Any]): P[String] = p.with1 *> (p1.string | (P.until(p1) <* p1))

  extension (str: String) private def prefix: P[String] = P.string(s"$str ").endWith(R.lf)
