package chess

case class Division(mid: Option[Int], end: Option[Int])

object Divider {

  def apply(replay: Replay): Division = {

    val boards = replay.chronoMoves.map { _.before }

    val midGame = boards.toStream.map(i =>
      (mixedness(i), majorsAndMinors(i), backRankSparse(i))
    ).indexWhere(i => i._1 > 150 || i._2 <= 10 || i._3 == true)

    val endGame = boards.toStream.map(majorsAndMinors).indexWhere(_ <= 6)

    Division(
      if (midGame < endGame || endGame == -1) indexOption(midGame) else None,
      indexOption(endGame)
    )
  }

  private def majorsAndMinors(board: Board): Int = board.pieces.values.map {
    _.role match {
      case Queen  => 1
      case Bishop => 1
      case Knight => 1
      case Rook   => 1
      case _      => 0
    }
  }.sum

  private def backRankSparse(board: Board): Boolean = {
    // Sparse back-rank indicates that pieces have been developed
    val white = (1 to 8).flatMap(x =>
      board(x, 1).map(piece =>
        if (piece is Color.white) 1 else 0
      )
    ).sum

    val black = (1 to 8).flatMap(x =>
      board(x, 8).map(piece =>
        if (piece is Color.black) 1 else 0
      )
    ).sum

    (black <= 3 || white <= 3)
  }

  private def score(white: Int, black: Int, x: Int, y: Int): Int = (white, black) match {
    case (0, 0) => 0

    case (1, 0) => 1 + (8 - y)
    case (2, 0) => if (y > 2) 2 + (y - 2) else 0
    case (3, 0) => if (y > 1) 3 + (y - 1) else 0
    case (4, 0) => if (y > 1) 3 + (y - 1) else 0 // group of 4 on the homerow = 0

    case (0, 1) => 1 + y
    case (1, 1) => 5 + (3 - y).abs
    case (2, 1) => 4 + y
    case (3, 1) => 5 + y

    case (0, 2) => if (y < 6) 2 + (6 - y) else 0
    case (1, 2) => 4 + (6 - y)
    case (2, 2) => 7

    case (0, 3) => if (y < 7) 3 + (7 - y) else 0
    case (1, 3) => 5 + (6 - y)

    case (0, 4) => if (y < 7) 3 + (7 - y) else 0

    case _      => 0
  }

  private def mixedness(board: Board): Int = {
    (1 to 7).flatMap(y =>
      (1 to 7).map(x =>
        {
          val cell = (0 to 1).flatMap(yp =>
            (0 to 1).map(xp =>
              board(x + xp, y + yp).map(
                piece =>
                  if (piece is Color.white) 1 else -1
              ).sum
            )
          ).groupBy(i => i).mapValues(_.size)

          score(cell.getOrElse(1, 0), cell.getOrElse(-1, 0), x, y)
        }
      )
    ).sum
  }

  private def indexOption(index: Int) = if (index == -1) None else Some(index)
}
