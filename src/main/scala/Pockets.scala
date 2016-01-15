package chess

case class Pocket(roles: List[Role])

case class Pockets(white: Pocket, black: Pocket)
