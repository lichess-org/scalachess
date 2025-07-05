package chess
package json

import chess.format.pgn.{ Glyph, Glyphs }
import chess.format.{ Uci, UciCharPair }
import chess.opening.Opening
import chess.variant.Crazyhouse
import play.api.libs.json.{ Json as PlayJson, * }
import scalalib.json.Json as LibJson
import scalalib.json.Json.given

object Json:

  given Writes[chess.Color] = LibJson.writeAs(_.name)

  given Reads[Uci]  = LibJson.optRead(Uci.apply)
  given Writes[Uci] = LibJson.writeAs(_.uci)

  given OWrites[Crazyhouse.Pocket] = OWrites: p =>
    JsObject:
      p.flatMap((role, nb) => Option.when(nb > 0)(role.name -> JsNumber(nb)))

  given OWrites[Crazyhouse.Data] = OWrites: v =>
    PlayJson.obj("pockets" -> v.pockets.all)

  given Writes[UciCharPair] with
    def writes(ucp: UciCharPair) = JsString(ucp.toString)

  given Writes[Square] with
    def writes(square: Square) = JsString(square.key)

  given Writes[Opening] with
    def writes(o: Opening) = PlayJson.obj("eco" -> o.eco, "name" -> o.name)

  given Writes[Glyph]  = PlayJson.writes[Glyph]
  given Writes[Glyphs] = Writes[Glyphs]: gs =>
    PlayJson.toJson(gs.toList)

  given Writes[Centis] = Writes: clock =>
    JsNumber(clock.centis)

  given Writes[Map[Square, Bitboard]] with
    def writes(dests: Map[Square, Bitboard]) = JsString(destString(dests))

  given OWrites[Division] = OWrites: o =>
    o.middle.fold(PlayJson.obj())(m => PlayJson.obj("middle" -> m)) ++
      o.end.fold(PlayJson.obj())(e => PlayJson.obj("end" -> e))

  given OWrites[CorrespondenceClock] = OWrites: c =>
    PlayJson.obj(
      "daysPerTurn" -> c.daysPerTurn,
      "increment"   -> c.increment,
      "white"       -> c.whiteTime,
      "black"       -> c.blackTime
    )

  given OWrites[chess.opening.Opening.AtPly] = OWrites: o =>
    PlayJson.obj(
      "eco"  -> o.opening.eco,
      "name" -> o.opening.name,
      "ply"  -> o.ply
    )

  def destString(dests: Map[Square, Bitboard]): String =
    val sb    = new java.lang.StringBuilder(80)
    var first = true
    dests.foreach: (orig, dests) =>
      if first then first = false
      else sb.append(" ")
      sb.append(orig.asChar)
      dests.foreach(d => sb.append(d.asChar))
    sb.toString
