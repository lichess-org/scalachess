package chess
package json

import format.Uci
import play.api.libs.json.{ Json as PlayJson, * }
import scalalib.json.Json as ScalaJson
import variant.Crazyhouse

object Json:

  given Writes[chess.Color] = ScalaJson.writeAs(_.name)

  given Reads[Uci] = ScalaJson.optRead(Uci.apply)
  given Writes[Uci] = ScalaJson.writeAs(_.uci)

  given ScalaJson.NoJsonHandler[Square] with {}

  given OWrites[Crazyhouse.Pocket] = OWrites: p =>
    JsObject:
      p.flatMap((role, nb) => Option.when(nb > 0)(role.name -> JsNumber(nb)))

  given OWrites[Crazyhouse.Data] = OWrites: v =>
    PlayJson.obj("pockets" -> v.pockets.all)
