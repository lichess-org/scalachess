package chess
package opening

opaque type Eco = String
object Eco extends OpaqueString[Eco]

opaque type OpeningName = String
object OpeningName extends OpaqueString[OpeningName]

// 1. d4 Nf6 2. c4 e6 3. g3
opaque type PgnMovesStr = String
object PgnMovesStr extends OpaqueString[PgnMovesStr]

// d2d4 g8f6 c2c4 e7e6 g2g3
opaque type UcisStr = String
object UcisStr extends OpaqueString[UcisStr]

opaque type OpeningKey = String
object OpeningKey extends OpaqueString[OpeningKey]:
  export Opening.nameToKey as fromName

case class OpeningFamily(name: OpeningName):
  lazy val key = Opening nameToKey name

opaque type OpeningVariation = String
object OpeningVariation extends OpaqueString[OpeningVariation]
