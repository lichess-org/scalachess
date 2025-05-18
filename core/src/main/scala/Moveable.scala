package chess

/**
  * A representation of a move or drop in chess.
  *
  * It can be applied to a specific position to get a MoveOrDrop.
  * It is either an Uci or a parsed San
  */
trait Moveable:
  def apply(position: Position): Either[ErrorStr, MoveOrDrop]
  def rawString: Option[String]
