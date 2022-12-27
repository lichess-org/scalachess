package chess

import chess.format.EpdFen
import chess.Pos.*
import chess.variant.*

import chess.format.EpdFen
import chess.variant.Crazyhouse
import chess.format.pgn.SanStr

class DT extends ChessTest:

  "Crazyhouse" should {
    "prod 50 games accumulate hash" in {
      val gameMoves: List[List[SanStr]] = format.pgn.Fixtures.prod50crazyhouse.map { g =>
        SanStr from g.split(' ').toList
      }
      def runOne(moves: List[SanStr]) =
        Replay.gameMoveWhileValid(moves, format.Fen.initial, Crazyhouse)
      def hex(buf: Array[Byte]): String = buf.map("%02x" format _).mkString
      val g                             = gameMoves.map(runOne)
      g.exists(_._3.nonEmpty) must beFalse
      val m8  = java.security.MessageDigest getInstance "MD5"
      val m16 = java.security.MessageDigest getInstance "MD5"
      val h   = Hash(16)
      g.foreach {
        _._2.foreach { x =>
          val ph = h(x._1.situation)
          m8.update(PositionHash.value(ph).slice(0, 8))
          m16.update(PositionHash value ph)
        }
      }
      hex(m8.digest) must beEqualTo("fcf5867ad3324c4be6d28108ff27212c")
      hex(m16.digest) must beEqualTo("80c4edf5fbd41eff78d3d563777beb61")
    }

    // "destinations prod bug on game VVXRgsQT" in {
    //   import chess.Pos.*
    //   chess
    //     .Game(
    //       Crazyhouse.some,
    //       EpdFen("r2q1b1r/p2k1Ppp/2p2p2/4p3/P2nP2n/3P1PRP/1PPB1K1q~/RN1Q1B2/Npb w - - 40 21").some
    //     )
    //     .situation
    //     .destinations must_== Map(
    //     F2 -> List(E3, E1),
    //     G3 -> List(G2),
    //     F1 -> List(G2)
    //   )
    // }

  }
