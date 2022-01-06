package chess
package format.pgn

class ErrorMessagesTest extends ChessTest {

  val parser = Parser.full _

  def parseMove(str: String) = Parser.MoveParser(str)


  "Invalid move" should {
    "yay" in {
      val e =
        """[abc "def"]
          |
          |1. e4 { hello world } Nc6
          |2. Bg0
          |""".stripMargin
      parser(e) must beValid
    }
  }

  "Lichess does not support null moves" should {
    "yay" in {
      val e =
        """[abc "def"]
          |
          |1. e5 { hello world } --
          |2. c6
          |""".stripMargin
      parser(e) must beValid
    }
  }
}