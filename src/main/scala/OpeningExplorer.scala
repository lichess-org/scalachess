package chess

object OpeningExplorer {

  private type Move = String

  case class Opening(code: String, name: String, size: Int) {

    def fullName = s"$code $name"

    override def toString = s"$code $name ($size)"
  }

  case class Branch(
      moves: Map[Move, Branch] = Map.empty,
      opening: Option[Opening] = None) {

    def get(move: Move) = moves get move

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

    override def toString = opening.fold("")(_.toString)
  }

  def openingOf(moves: List[String]): Option[Opening] = {
    def next(
      branch: Branch,
      moves: List[Move],
      last: Option[Opening]): Option[Opening] =
      moves match {
        case Nil => branch.opening orElse last
        case m :: ms => (branch get m).orElse(branch get "**").fold(last) { b =>
          next(b, ms, b.opening orElse last)
        }
      }
    next(tree, moves, none)
  }

  val tree: Branch = Openings.db.foldLeft(Branch()) {
    case (tree, (code, name, moves)) =>
      val moveList = moves.split(' ').toList
      tree.add(moveList, Opening(code, name, moveList.size))
  }
}
