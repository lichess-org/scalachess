package chess
package variant

import format.Uci

import scalaz.Validation.FlatMap._

case object Crazyhouse extends Variant(
  id = 10,
  key = "crazyhouse",
  name = "Crazyhouse",
  shortName = "crazy",
  title = "Every time a piece is captured the capturing player gets a piece of the same type and of their color in their reserve.",
  standardInitialPosition = true) {

  override def drop(situation: Situation, role: Role, pos: Pos): Valid[Drop] = for {
    d1 <- situation.board.crazyData toValid "Board has no crazyhouse data"
    _ <- d1 validIf (role == Pawn && pos.y == 1 || pos.y == 8, s"Can't drop $role on $pos")
    piece = Piece(situation.color, role)
    d2 <- d1.drop(piece, pos) toValid s"No $piece to drop"
    board1 <- situation.board.place(piece, pos) toValid s"Can't drop $role on $pos, it's occupied"
  } yield Drop(
    piece = piece,
    pos = pos,
    before = situation.board,
    after = board1 withCrazyData d2)

  override def finalizeBoard(board: Board, uci: Uci, capture: Option[Piece]): Board =
    board.crazyData.fold(board) { data =>
      val d1 = capture.fold(data)(data.store)
      val d2 = uci match {
        case Uci.Move(_, dest, Some(_)) => d1 promote dest
        case Uci.Move(orig, dest, _)    => d1.move(orig, dest)
        case _                          => d1
      }
      board withCrazyData d2
    }

  case class Data(
      pockets: Pockets,
      // in crazyhouse, a promoted piece becomes a pawn
      // when captured and put in the pocket.
      // there we need to remember which pieces are issued from promotions.
      // we do that by tracking their positions on the board.
      promoted: Set[Pos]) {

    def drop(piece: Piece, pos: Pos): Option[Data] =
      pockets take piece map { nps =>
        copy(pockets = nps)
      }

    def store(piece: Piece) = copy(pockets = pockets store piece)

    def promote(pos: Pos) = copy(promoted = promoted + pos)

    def move(orig: Pos, dest: Pos) = copy(
      promoted = if (promoted(orig)) promoted - orig + dest else promoted
    )
  }

  case class Pockets(white: Pocket, black: Pocket) {

    def take(piece: Piece): Option[Pockets] = piece.color.fold(
      white take piece.role map { np => copy(white = np) },
      black take piece.role map { np => copy(black = np) })

    def store(piece: Piece) = copy(
      white = piece.color.fold(white store piece.role, white),
      black = piece.color.fold(black, black store piece.role))
  }

  case class Pocket(roles: List[Role]) {

    def take(role: Role) =
      if (roles contains role) Some(copy(roles = roles diff List(role)))
      else None

    def store(role: Role) = copy(roles = role :: roles)
  }
}
