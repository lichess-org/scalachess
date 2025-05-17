package chess

trait Moveable:
  def apply(position: Position): Either[ErrorStr, MoveOrDrop]
  def rawString: Option[String]
