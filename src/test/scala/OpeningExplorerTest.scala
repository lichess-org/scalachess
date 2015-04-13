package chess

class OpeningExplorerTest extends ChessTest {

  def name(g: String) = OpeningExplorer openingOf g.split(' ').toList map (_.name)

  def code(g: String) = OpeningExplorer openingOf g.split(' ').toList map (_.code)

  val g1 = "d4 Nf6 e4 Nxe4 f3 Nd6 g3"
  g1 in {
    name(g1) must_== Some("Indian Game, Omega Gambit")
  }
  val g2 = "e4 e5 Nf3 Nf6"
  g2 in {
    name(g2) must_== Some("Russian Game, General")
  }
  val g3 = "e4 e5"
  g3 in {
    name(g3) must_== Some("King Pawn Game, General")
  }
  val g4 = "e4 e5 b3 Nc6"
  g4 in {
    name(g4) must_== Some("King Pawn Opening, Not Named")
  }
  val g5 = "e4 e5 b3 Nc6 Nc3"
  g5 in {
    name(g5) must_== Some("King Pawn Opening, Not Named")
  }
  val g6 = "e4 Nf6 e5 Nd5 d4"
  g6 in {
    code(g6) must_== Some("B02")
  }
  val g7 = "e4 Nf6 e5 Nd5 d4 d6 Nf3"
  g7 in {
    code(g7) must_== Some("B04")
  }
  val g8 = "e4 Nf6 e5 Nd5 d4 d6 a3"
  g8 in {
    code(g8) must_== Some("B03")
  }
  val transvestite1 = "d3 d5 Kd2 e6 Qe1 Qh4 Kd1 Nf6"
  transvestite1 in {
    name(transvestite1) must_== Some("Transvestite Attack, Courteous, Gay Variation")
  }
  val transvestite2 = "d6 ?? Qd7 ?? Kd8 ?? Qe8 ?? ?? ??"
  transvestite2 in {
    name(transvestite2) must_== Some("Transvestite Defence, Discourteous, Straight Variation")
  }
}
