package chess

object OpeningExplorer {

  type Move = String

  case class Opening(code: String, name: String) {

    def fullName = s"$code $name"
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
      margin + toString + "\n" + (moves map {
        case (m, b) => margin + m + b.render(margin + "  ")
      } mkString "\n")

    override def toString = opening.fold("-") { o => o.code + ": " + o.name }
  }

  def openingOf(moves: List[String]): Option[Opening] = {

    def next(
      branch: Branch,
      moves: List[Move],
      last: Option[Opening]): Option[Opening] =
      moves match {
        case Nil     => branch.opening orElse last
        case m :: ms => (branch get m).fold(last) { b => next(b, ms, b.opening orElse last) }
      }
    next(tree, moves, none)
  }

  val tree: Branch = Openings.db.foldLeft(Branch()) {
    case (tree, (code, name, moves)) => tree.add(
      moves.split(' ').toList,
      Opening(code, name)
    )
  }
}
