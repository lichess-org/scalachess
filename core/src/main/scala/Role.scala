package chess

sealed trait Role:
  val forsyth: Char
  lazy val forsythUpper: Char              = forsyth.toUpper
  lazy val pgn: Char                       = forsythUpper
  lazy val name                            = toString.toLowerCase
  inline def forsythBy(color: Color): Char =
    if color.white then forsythUpper else forsyth

sealed trait PromotableRole extends Role

/** Promotable in antichess. */
case object King extends PromotableRole:
  val forsyth = 'k'

case object Queen extends PromotableRole:
  val forsyth = 'q'

case object Rook extends PromotableRole:
  val forsyth = 'r'

case object Bishop extends PromotableRole:
  val forsyth = 'b'

case object Knight extends PromotableRole:
  val forsyth = 'n'

case object Pawn extends Role:
  val forsyth = 'p'

object Role:

  val all: List[Role]                                   = List(King, Queen, Rook, Bishop, Knight, Pawn)
  val allPromotable: List[PromotableRole]               = List(Queen, Rook, Bishop, Knight, King)
  val allByForsyth: Map[Char, Role]                     = all.mapBy(_.forsyth)
  val allByPgn: Map[Char, Role]                         = all.mapBy(_.pgn)
  val allByName: Map[String, Role]                      = all.mapBy(_.name)
  val allPromotableByName: Map[String, PromotableRole]  = allPromotable.mapBy(_.toString)
  val allPromotableByForsyth: Map[Char, PromotableRole] = allPromotable.mapBy(_.forsyth)
  val allPromotableByPgn: Map[Char, PromotableRole]     = allPromotable.mapBy(_.pgn)

  def forsyth(c: Char): Option[Role] = allByForsyth.get(c)

  def promotable(c: Char): Option[PromotableRole] =
    allPromotableByForsyth.get(c)

  def promotable(name: String): Option[PromotableRole] =
    allPromotableByName.get(name.capitalize)

  def promotable(name: Option[String]): Option[PromotableRole] =
    name.flatMap(promotable)

  def valueOf(r: Role): Option[Int] =
    r match
      case Pawn   => Option(1)
      case Knight => Option(3)
      case Bishop => Option(3)
      case Rook   => Option(5)
      case Queen  => Option(9)
      case King   => None
