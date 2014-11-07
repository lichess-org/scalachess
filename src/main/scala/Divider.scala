package chess

object Divider {

  case class Cell(white: Int, black: Int, x: Int, y: Int) {
    def score: Int = (white, black) match {
      case (0, 0) => 0

      case (1, 0) => 1 + (8-y)
      case (2, 0) => if (y > 2) 2 + (y - 2) else 0
      case (3, 0) => if (y > 1) 3 + (y - 1) else 0
      case (4, 0) => if (y > 1) 4 + (y - 1) else 0 // group of 4 on the homerow = 0

      case (0, 1) => 1 + y
      case (1, 1) => 5
      case (2, 1) => 3 + y
      case (3, 1) => 4 + y

      case (0, 2) => if (y < 6) 2 + (6 - y) else 0
      case (1, 2) => 3 + (6 - y)
      case (2, 2) => 7

      case (0, 3) => if (y < 7) 3 + (7 - y) else 0
      case (1, 3) => 4 + (6 - y)

      case (0, 4) => if (y < 7) 4 + (7 - y) else 0
    }
  }

  def indexOption(index: Int) = if (index == -1) None else Some(index)

  def apply(replay: Replay): (Option[Int], Option[Int]) = {
    val boards = replay.chronoMoves.map { _.before }
      
    (
      indexOption(boards.map(mixedness).indexWhere( _ > 230))
    ,
      indexOption(boards.map(value).indexWhere( _ <= 40))
    )

  }

  def value(board: Board): Int = {
    (1 to 8).map( y => 
      (1 to 8).map( x => 
        board(x, y).map(
          _.role match {
            case King => 0
            case Queen => 9
            case Bishop => 3
            case Knight => 3
            case Rook => 5
            case Pawn => 1
          }
        ).sum
      )
    ).flatten.sum
  }

  def mixedness(board: Board): Int = {
    (1 to 7).flatMap( y =>
      (1 to 7).map( x =>
        {
          val cell = (0 to 1).flatMap( yp =>
            (0 to 1).map( xp =>
              board(x + xp, y + yp).map(
                piece =>
                  if (piece is Color.white) 1 else -1
              ).sum
            )
          ).groupBy(i => i).mapValues(_.size)

          Cell(cell.getOrElse(1, 0), cell.getOrElse(-1, 0), x, y).score
        }
      )
    ).sum + value(board)
  }
}
