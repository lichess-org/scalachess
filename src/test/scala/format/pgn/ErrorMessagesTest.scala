package chess
package format.pgn

class ErrorMessagesTest extends ChessTest {

  args(skipAll = true)

  val parser = Parser.full _

  "Invalid move" should {
    "yay" in {
      val e =
        """[abc "def"]
          |
          |1. e4 { hello world } Nc6
          |2. bla
          |""".stripMargin
      parser(e) must beValid
    }
  }

  "Lichess does not support null moves" should {
    "yay" in {
      val e =
        """[abc "def"]
          |
          |1. e4 { hello world } --
          |2. c6
          |""".stripMargin
      parser(e) must beValid
    }
  }

  "too many glyphs" should {
    "failed" in {
      val e =
        """[abc "def"]
          |
          |1. e4 { hello world } c5????
          |2. c6
          |""".stripMargin
      parser(e) must beInvalid
    }
  }

  "invalid glyphs" should {
    "failed" in {
      val e =
        """[abc "def"]
          |
          |1. e4 { hello world } c5??@@
          |2. c6
          |""".stripMargin
      parser(e) must beValid
    }
  }

  "Bad comment" should {
    "failed" in {
      val e =
        """[abc "def"]
          |
          |1. e4 { hello world
          |2. c6
          |""".stripMargin
      parser(e) must beValid
    }
  }

  "bad tags" should {
    "failed" in {
      val e =
        """|[ab cdef"]
           |
           |1. e4 { hello world }  c5??
           |2. c6
           |""".stripMargin
      parser(e) must beInvalid
    }
  }

  "invalid promotion" should {
    "failed" in {
      val e =
        """|[abc "def"]
           |
           |1. e4 h8=L
           |2. c6
           """.stripMargin
      parser(e) must beValid
    }
  }

}
