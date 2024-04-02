package chess
package json

import chess.format.Uci
import chess.variant.Crazyhouse
import play.api.libs.json.{ Json as PlayJson, * }
import scalalib.json.Json
import scalalib.json.Json.*

object Json:

  given Writes[chess.Color] = writeAs(_.name)

  given Reads[Uci] = Reads
    .of[String]
    .flatMapResult: str =>
      JsResult.fromTry(Uci(str).toTry(s"Invalid UCI: $str"))
  given Writes[Uci] = writeAs(_.uci)

  given NoJsonHandler[Square] with {}

  given OWrites[Crazyhouse.Pocket] = OWrites: p =>
    JsObject:
      p.flatMap((role, nb) => Option.when(nb > 0)(role.name -> JsNumber(nb)))

  given OWrites[Crazyhouse.Data] = OWrites: v =>
    PlayJson.obj("pockets" -> v.pockets.all)
