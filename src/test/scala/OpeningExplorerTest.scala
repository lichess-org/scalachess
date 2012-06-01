package chess

class OpeningExplorerTest extends ChessTest {

  def name(g: String) = OpeningExplorer openingOf g map (_.name)

  def code(g: String) = OpeningExplorer openingOf g map (_.code)

  val g1 = "d4 Nf6 e4 Nxe4 f3 Nd6 g3" 
  g1 in {
    name(g1) must_== Some("Indian Defense")
  }
  val g2 = "e4 e5 Nf3 Nf6" 
  g2 in {
    name(g2) must_== Some("Petrov Defense")
  }
  val g3 = "e4 e5" 
  g3 in {
    name(g3) must_== Some("King's Pawn Game")
  }
  val g4 = "e4 e5 b3 Nc6" 
  g4 in {
    name(g4) must_== Some("King's Pawn Game")
  }
  val g5 = "e4 e5 b3 Nc6 Nc3"
  g5 in {
    name(g5) must_== Some("King's Pawn Game")
  }
  val g6 = "e4 Nf6 e5 Nd5 d4"
  g6 in {
    code(g6) must_== Some("B03")
  }
  val g7 = "e4 Nf6 e5 Nd5 d4 d6 Nf3"
  g7 in {
    code(g7) must_== Some("B04")
  }
  val g8 = "e4 Nf6 e5 Nd5 d4 d6 a3"
  g8 in {
    code(g8) must_== Some("B03")
  }
}
