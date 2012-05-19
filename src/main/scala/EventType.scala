package chess

sealed trait EventType {

  lazy val name = toString.toLowerCase

  def casual = this == EventType.Casual

  def rated = this == EventType.Rated
}

object EventType {

  case object Casual extends EventType 

  case object Rated extends EventType 

  val all = List(Casual, Rated)

  val default = Casual

  def apply(rated: Boolean) = rated.fold(Rated, Casual)
}
