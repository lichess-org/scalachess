package chess
package format
package pgn

// Nf6
opaque type SanStr = String
object SanStr extends OpaqueString[SanStr]

// 1. d4 Nf6 2. c4 e6 3. g3
opaque type PgnMovesStr = String
object PgnMovesStr extends OpaqueString[PgnMovesStr]

// full PGN game
opaque type PgnStr = String
object PgnStr extends OpaqueString[PgnStr]

opaque type Comment = String
object Comment extends TotalWrapper[Comment, String]:
  extension (c: Comment) def trimNonEmpty: Option[Comment] = Option.unless(c.isBlank)(c.trim)
  extension (cs: List[Comment]) def trimNonEmpty: List[Comment] = cs.flatMap(Comment.trimNonEmpty)

opaque type InitialComments = List[Comment]
object InitialComments extends TotalWrapper[InitialComments, List[Comment]]:
  val empty: InitialComments = Nil

  extension (ip: InitialComments) inline def comments: List[Comment] = ip
