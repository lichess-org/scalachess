package chess
package format.pgn

import scala.language.implicitConversions

class ErrorMessagesTest extends ChessTest:

  args(skipAll = true)

  val parser = Parser.full

  "Invalid move" should:
    "fail" in:
      val e =
        """[abc "def"]
          |
          |1. e4 { hello world } Nc6
          |2. bla
          |""".stripMargin
      parser(e) must beValid

  "Null moves are not supported" should:
    "fail" in:
      val e =
        """[abc "def"]
          |
          |1. e4 { hello world } --
          |2. c6
          |""".stripMargin
      parser(e) must beValid

  "too many glyphs" should:
    "fail" in:
      val e =
        """[abc "def"]
          |
          |1. e4 { hello world } c5????
          |2. c6
          |""".stripMargin
      parser(e) must beInvalid

  "invalid glyphs" should:
    "fail" in:
      val e =
        """[abc "def"]
          |
          |1. e4 { hello world } c5??@@
          |2. c6
          |""".stripMargin
      parser(e) must beValid

  "bad comment" should:
    "fail" in:
      val e =
        """[abc "def"]
          |
          |1. e4 { hello world
          |2. c6
          |""".stripMargin
      parser(e) must beValid

  "invalid tags 1" should:
    "failed" in:
      val e =
        """|[ab "cdef]
           |
           |1. e4 { hello world }  c5??
           |2. c6
           |""".stripMargin
      parser(e) must beValid

  "invalid tags 2" should:
    "failed" in:
      val e =
        """|[ab "cdef"]    [123]
           |
           |1. e4 { hello world }  c5??
           |2. c6
           |""".stripMargin
      parser(e) must beValid

  "invalid promotion" should:
    "failed" in:
      val e =
        """|[abc "def"]
           |
           |1. e4 h8=L
           |2. c6
           """.stripMargin
      parser(e) must beValid
