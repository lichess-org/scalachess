package chess

enum Status(val id: Int):

  val name = s"${toString.head.toLower}${toString.tail}"

  inline def is(inline s: Status): Boolean = this == s
  inline def is(inline f: Status.type => Status): Boolean = is(f(Status))

  inline infix def >=(inline s: Status): Boolean = id >= s.id
  inline infix def >(inline s: Status): Boolean = id > s.id
  inline infix def <=(inline s: Status): Boolean = id <= s.id
  inline infix def <(inline s: Status): Boolean = id < s.id

  case Created extends Status(10)
  case Started extends Status(20)
  case Aborted extends Status(25) // from this point the game is finished
  case Mate extends Status(30)
  case Resign extends Status(31)
  case Stalemate extends Status(32)
  case Timeout extends Status(33) // when player leaves the game
  case Draw extends Status(34)
  case Outoftime extends Status(35) // clock flag
  case Cheat extends Status(36)
  case NoStart extends Status(37) // the player did not make the first move in time
  case InsufficientMaterialClaim extends Status(38)
  case UnknownFinish extends Status(50) // we don't know why the game ended
  case VariantEnd extends Status(60) // the variant has a special ending

object Status:

  val all = values.toList

  given Ordering[Status] = Ordering.by(_.id)

  val finishedNotCheated = all.filter { s =>
    s.id >= Mate.id && s.id != Cheat.id
  }

  val finishedWithWinner = List(Mate, Resign, Timeout, Outoftime, Cheat, NoStart, VariantEnd)

  val byId = all.mapBy(_.id)

  def apply(id: Int): Option[Status] = byId.get(id)
