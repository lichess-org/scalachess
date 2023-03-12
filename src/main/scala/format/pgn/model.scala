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
