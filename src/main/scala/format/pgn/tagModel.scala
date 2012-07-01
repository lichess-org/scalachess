package chess
package format.pgn

sealed abstract class Tag(name: String, value: String)

case class Fen(value: String) extends Tag("fen", value)

case class Variant(value: String) extends Tag("variant", value)

case class Unknown(name: String, value: String) extends Tag(name, value)
