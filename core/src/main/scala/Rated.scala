package chess

opaque type Rated = Boolean

object Rated extends YesNo[Rated]:

  extension (r: Rated)
    def name: String = if r then "rated" else "casual"
    def id: Int      = if r then 1 else 0

  val all: List[Rated] = List(No, Yes)

  val byId: Map[Int, Rated] = Map(0 -> No, 1 -> Yes)

  def apply(id: Int): Option[Rated] = byId.get(id)

  val default: Rated = No

  def orDefault(id: Int): Rated = byId.getOrElse(id, default)
