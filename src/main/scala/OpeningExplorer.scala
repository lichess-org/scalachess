package chess

object OpeningExplorer {

  private type Move = String
  private val any: Move = "**"

  case class Branch(
      moves: Map[Move, Branch] = Map.empty,
      opening: Option[Opening] = None) {

    def get(move: Move): Option[Branch] = {
      val precise = moves get move
      val wildcard = moves get any
      wildcard.fold(precise) { wild =>
        precise.fold(wild)(_ merge wild).some
      }
    }

    def apply(move: Move) = get(move) getOrElse Branch()

    def add(moves: List[Move], opening: Opening): Branch = moves match {
      case Nil            => this
      case move :: Nil    => this.updated(move, apply(move) set opening)
      case move :: others => this.updated(move, apply(move).add(others, opening))
    }

    def updated(k: Move, v: Branch) = copy(moves = moves.updated(k, v))

    def set(o: Opening) = copy(opening = Some(o))

    def render(margin: String = ""): String =
      "   " + toString + "\n" + (moves map {
        case (m, b) => margin + m + b.render(margin + " ")
      } mkString "\n")

    private def merge(other: Branch) = Branch(
      moves = other.moves ++ moves,
      opening = opening orElse other.opening)

    override def toString = "branch " + opening.fold("")(_.toString) + moves.keys
  }

  def openingOf(moves: List[String]): Option[Opening] = {
    def next(
      branch: Branch,
      moves: List[Move],
      last: Option[Opening]): Option[Opening] =
      moves match {
        case Nil => branch.opening orElse last
        case m :: ms => (branch get m).fold(last) { b =>
          next(b, ms, b.opening orElse last)
        }
      }
    next(tree, moves, none)
  }

  val tree: Branch = OpeningDB.db.foldLeft(Branch()) {
    case (tree, opening) => tree.add(opening.moveList, opening)
  }
}
