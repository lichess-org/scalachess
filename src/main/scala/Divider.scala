package chess

object Divider {

  case class Cell(white: Int, black: Int, x: Int, y: Int) {
    def score: Int = (white, black) match {
      case (0, 0) => 0

      case (1, 0) => 1
      case (2, 0) => 2 * (y - 2)
      case (3, 0) => 3 * (y - 1)
      case (4, 0) => 4 * (y - 1) // group of 4 on the homerow = 0

      case (0, 1) => 1
      case (1, 1) => 3
      case (2, 1) => 3 + y
      case (3, 1) => 4 + y

      case (0, 2) => 2 * (6 - y)
      case (1, 2) => 3 + (6 - y)
      case (2, 2) => 7

      case (0, 3) => 3 * (7 - y)
      case (1, 3) => 4 + (6 - y)

      case (0, 4) => 4 * (7 - y)
    }
  }

  def apply(replay: Replay): (Option[Int], Option[Int]) = {
    val boards = replay.chronoMoves.map { _.before }
      
    (
      boards.map(mixedness)indexWhere( _ > 40) match {
        case -1 => None
        case a => Option(a + 1)
      }
    ,
      boards.map(sparsity)indexWhere( _ <= 20) match {
        case -1 => None
        case a => Option(a + 1)
      }
    )

  }

  def sparsity(board: Board): Int = {
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
    (1 to 7).map( y =>
      (1 to 7).map( x =>
        (0 to 1).map( yp =>
          (0 to 1).map( xp =>
            board(x + xp, y + yp).map(
              piece =>
                if (piece is Color.white) 1 else -1
            ).sum
          )
        ).flatten.groupBy(i => i).mapValues(_.size) match {
          case cell => Cell(cell.getOrElse(1, 0), cell.getOrElse(-1, 0), x, y).score
        }
      )
    ).flatten.sum
  }
}
