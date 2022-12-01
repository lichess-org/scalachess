package chess
package opening

opaque type Eco = String
object Eco extends OpaqueString[Eco]

opaque type OpeningName = String
object OpeningName extends OpaqueString[OpeningName]

opaque type SansStr = String
object SansStr extends OpaqueString[SansStr]

opaque type UcisStr = String
object UcisStr extends OpaqueString[UcisStr]

opaque type OpeningKey = String
object OpeningKey extends OpaqueString[OpeningKey]:
  export Opening.nameToKey as fromName

case class OpeningFamily(name: OpeningName):
  lazy val key = Opening nameToKey name

opaque type OpeningVariation = String
object OpeningVariation extends OpaqueString[OpeningVariation]
