package chess
package variant

case object Standard
    extends Variant(
      id = Variant.Id(1),
      key = Variant.LilaKey("standard"),
      uciKey = Variant.UciKey("chess"),
      name = "Standard",
      shortName = "Std",
      title = "Standard rules of chess (FIDE)",
      standardInitialPosition = true
    ):

  val pieces: Map[Pos, Piece] = Variant.symmetricRank(backRank)

  override def validMoves(situation: Situation): List[Move] =
    val enPassantMoves = situation.genEnPassant(situation.us & situation.board.pawns)
    situation.ourKings.headOption
      .fold(Nil)(king =>
        val checkers = situation.board.board.attackers(king, !situation.color)
        val candidates =
          if checkers.isEmpty then
            val targets = ~situation.us
            situation.genNonKing(targets) ::: situation.genSafeKing(
              targets
            ) ::: situation.genCastling ::: enPassantMoves
          else situation.genEvasions(checkers) ::: enPassantMoves
        if situation.sliderBlockers.nonEmpty || enPassantMoves.nonEmpty then
          candidates.filter(situation.isSafe(king, _, situation.sliderBlockers))
        else candidates
      )
