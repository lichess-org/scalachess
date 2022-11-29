package chess
package variant

case object Standard
    extends Variant(
      id = 1,
      key = "standard",
      uciKey = "chess",
      name = "Standard",
      shortName = "Std",
      title = "Standard rules of chess (FIDE)",
      standardInitialPosition = true
    ):

  val pieces: Map[Pos, Piece] = Variant.symmetricRank(backRank)
