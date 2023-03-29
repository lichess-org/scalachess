package chess
package opening

import format.EpdFen
import format.pgn.SanStr
import org.specs2.mutable.Specification

class OpeningTest extends Specification:

  def searchStr(str: String) =
    OpeningDb search SanStr.from(str.split(' ').toList) map (_.opening)

  "search" should:
    "find nothing on invalid PGN" in:
      searchStr("e4 c5 Nf3 cxd4 d4 cxd4 Nxd4 e5 Nb5 d6 c4 a6 N5c3 Nf6 Be2 Be7") must beNone
    "find Kalashnikov" in:
      searchStr("e4 c5 Nf3 Nc6 d4 cxd4 Nxd4 e5 Nb5 d6 c4 a6 N5c3 Nf6 Be2 Be7") must beSome:
        (_: Opening).name == OpeningName("Sicilian Defense: Kalashnikov Variation")
    "ignore everything after a Crazyhouse drop" in:
      searchStr("e4 d5 exd5 Qxd5 Nc3 Qa5 d4 Nf6 Nf3 Bf5 @e5 @b4 Bd2") must beSome:
        (_: Opening).name == OpeningName("Scandinavian Defense: Classical Variation")
    "find Muzio" in:
      searchStr(
        "e4 e5 f4 exf4 Nf3 g5 Bc4 g4 O-O gxf3 Qxf3"
      ) must beSome:
        (_: Opening).name == OpeningName("King's Gambit Accepted: Muzio Gambit, Wild Muzio Gambit")
      searchStr(
        "e4 e5 f4 exf4 Nf3 g5 Bc4 g4 O-O gxf3 Qxf3 Nc6 Qxf4 f6 Nc3 d6 Nd5 Ne5 Bb3 Ng6 Nxf6+ Qxf6 Qxf6 Nxf6 Rxf6 Bd7 Bf7+ Ke7 Rf2 Be8 Bb3 Bg7 c3 Rf8 Rxf8 Kxf8 d4 Bf7 Bxf7 Kxf7 Bg5 c5 Rf1+ Kg8 d5 Re8 Re1 Rf8 Be3"
      ) must beSome:
        (_: Opening).name == OpeningName("King's Gambit Accepted: Muzio Gambit, Holloway Defense")
    "find Queen's Pawn" in:
      searchStr("d4") must beSome:
        (_: Opening).name == OpeningName("Queen's Pawn Game")
      val op = OpeningDb.search(SanStr.from(List("d4"))).get
      op.opening.name === OpeningName("Queen's Pawn Game")
      op.ply === Ply(1)
    "find Old Benoni Defense" in:
      searchStr("d4 c5 d5 e5") must beSome:
        (_: Opening).name == OpeningName("Benoni Defense: Old Benoni")
    "find by replay" in:
      "d4" >> :
        val replay = Replay(List(SanStr("d4")), None, variant.Standard).toOption.get.valid.toOption.get
        val op     = OpeningDb.search(replay).get
        op.opening.name === OpeningName("Queen's Pawn Game")
        op.ply === Ply(1)
      "full" >> :
        val replay = Replay(
          SanStr from "e4 e5 f4 exf4 Nf3 g5 Bc4 g4 O-O gxf3 Qxf3 Nc6 Qxf4 f6 Nc3 d6 Nd5 Ne5 Bb3 Ng6 Nxf6+ Qxf6 Qxf6 Nxf6 Rxf6 Bd7 Bf7+ Ke7 Rf2 Be8 Bb3 Bg7 c3 Rf8 Rxf8 Kxf8 d4 Bf7 Bxf7 Kxf7 Bg5 c5 Rf1+ Kg8 d5 Re8 Re1 Rf8 Be3"
            .split(' ')
            .toList,
          None,
          variant.Standard
        ).toOption.get.valid.toOption.get
        val op = OpeningDb.search(replay).get
        op.opening.name === OpeningName("King's Gambit Accepted: Muzio Gambit, Holloway Defense")
        op.ply === Ply(12)

  "by fen" should:
    "consider en passant" in:
      OpeningDb findByEpdFen EpdFen(
        "rnbqkbnr/pp1p1ppp/8/2pPp3/8/8/PPP1PPPP/RNBQKBNR w KQkq - 0 3"
      ) must beNone
      OpeningDb findByEpdFen EpdFen(
        "rnbqkbnr/pp1p1ppp/8/2pPp3/8/8/PPP1PPPP/RNBQKBNR w KQkq e6 0 3"
      ) must beSome
    "ignore empty crazyhouse pocket" in:
      OpeningDb findByEpdFen EpdFen(
        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR/ b KQkq - 0 1"
      ) must beSome:
        (_: Opening).name == OpeningName("King's Pawn Game")
      OpeningDb findByEpdFen EpdFen(
        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR[] b KQkq - 0 1"
      ) must beSome:
        (_: Opening).name == OpeningName("King's Pawn Game")
    "ignore crazyhouse pocket" in:
      OpeningDb findByEpdFen EpdFen(
        "rn2kb1r/ppp1pppp/5n2/q4b2/3P4/2N2N2/PPP2PPP/R1BQKB1R/Pp w KQkq - 3 6"
      ) must beSome:
        (_: Opening).name == OpeningName("Scandinavian Defense: Classical Variation")
      OpeningDb findByEpdFen EpdFen(
        "rn2kb1r/ppp1pppp/5n2/q4b2/3P4/2N2N2/PPP2PPP/R1BQKB1R[Pp] w KQkq - 3 6"
      ) must beSome:
        (_: Opening).name == OpeningName("Scandinavian Defense: Classical Variation")

  "nameToKey" in:
    "opening name" in:
      def nameToKey(n: String) = OpeningKey.fromName(OpeningName(n))
      import Opening.nameToKey
      nameToKey("Grünfeld Defense") must_== OpeningKey("Grunfeld_Defense")
      nameToKey("King's Pawn Game") must_== OpeningKey("Kings_Pawn_Game")
      nameToKey("Neo-Grünfeld Defense") must_== OpeningKey("Neo-Grunfeld_Defense")
      nameToKey(
        "Bishop's Opening: McDonnell Gambit, La Bourdonnais-Denker Gambit"
      ) must_== OpeningKey("Bishops_Opening_McDonnell_Gambit_La_Bourdonnais-Denker_Gambit")
