package chess
package format.pgn

import scala._

object Binary {

  object Write {

    def moves(str: String): List[Byte] = str.split(' ').toList flatMap move

    val SimplePawnMove = """[a-h][1-8]""".r

    def move(str: String): List[Byte] = str match {
      case SimplePawnMove ⇒ ???
    }
  }

  def read(bin: List[Byte]): String = ""
}

// from https://github.com/retronym/macrocosm/blob/master/src/main/scala/com/github/retronym/macrocosm/Macrocosm.scala
// credits: retronym
object Macrocosm {

  import scala.language.experimental.macros
  import scala.reflect.macros.Context

  class RichStringContext(sc: StringContext) {
    // This is how a non-macro version would be implemented.
    // def b() = {
    // val s = sc.parts.mkString
    // parseBinary(s).getOrElse(sys.error("invalid binary literal: " + s))
    // }

    /**
     * Binary literal integer
     *
     * {{{
     * scala> b"101010"
     * res0: Int = 42
     * }}}
     */
    def b(): Int = macro bImpl
  }

  def bImpl(c: Context)(): c.Expr[Int] = {
    def parseBinary(s: String): Int = {
      var i = s.length - 1
      var sum = 0
      var mult = 1
      while (i >= 0) {
        s.charAt(i) match {
          case '1' ⇒ sum += mult
          case '0' ⇒
          case x ⇒
            c.abort(c.enclosingPosition, "invalid binary literal")
        }
        mult *= 2
        i -= 1
      }
      sum
    }
  }
}
