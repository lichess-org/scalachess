package chess
package format

import scala.language.implicitConversions

class FenTest extends ChessTest:

  import pgn.Fixtures.*
  import pgn.Reader
  import Pos.*

  "write" should {
    "enpassant" in {
      val sit = Reader.full("d4 e5 d5 c5").flatMap(_.valid).toOption.get
      Fen.write(sit.state) === EpdFen("rnbqkbnr/pp1p1ppp/8/2pPp3/8/8/PPP1PPPP/RNBQKBNR w KQkq c6 0 3")
    }
    "enpassant 2" in {
      val sit = Reader.full("e4 c5 e5 f5").flatMap(_.valid).toOption.get
      Fen.write(sit.state) === EpdFen("rnbqkbnr/pp1pp1pp/8/2p1Pp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3")
    }
    "enpassant in game" in {
      val pgn =
        """1. e4 e5 2. Nf3 Bc5?? { (0.29 → 2.18) Blunder. Nc6 was best. } { C40 King's Pawn Game: Busch-Gass Gambit } (2... Nc6 3. Bb5 Nf6 4. O-O Nxe4 5. Re1 Nd6 6. Nxe5 Nxe5 7. Rxe5+) 3. c3? { (2.18 → 0.76) Mistake. Nxe5 was best. } (3. Nxe5 Nc6 4. Nxc6 dxc6 5. Qe2 Be6 6. d3 Qe7 7. Nc3 Bd4) 3... Bxf2+?? { (0.76 → 3.66) Blunder. d5 was best. } (3... d5 4. exd5 Qxd5 5. d4 exd4 6. cxd4 Bb4+ 7. Nc3 Nf6 8. Bd3) 4. Kxf2 Nf6 5. Bd3 Ng4+? { (2.87 → 5.46) Mistake. d5 was best. } (5... d5) 6. Kf1?! { (5.46 → 3.90) Inaccuracy. Kg1 was best. } (6. Kg1 O-O 7. Bc2 f5 8. exf5 d5 9. h3 Nh6 10. Nxe5 Nc6) 6... d6?! { (3.90 → 5.46) Inaccuracy. f5 was best. } (6... f5 7. exf5) 7. Qe2?! { (5.46 → 3.59) Inaccuracy. h3 was best. } (7. h3 Nf6 8. Bc2 O-O 9. Qe1 Nh5 10. g4 Nf4 11. d3 Ng6 12. Rh2 c5 13. Kg1 d5) 7... O-O 8. Bc2 f5 9. exf5 Rxf5? { (3.47 → 5.80) Mistake. d5 was best. } (9... d5 10. d3 Bxf5 11. Kg1 Nc6 12. h3 Nf6 13. Nh4 Be6 14. Bg5 Qd6 15. Nd2 h6 16. Bxf6) 10. Bxf5 Bxf5 11. Kg1 e4 12. Nd4 Qd7 13. Nxf5 Qxf5 14. b3?? { (6.28 → -0.68) Blunder. d4 was best. } (14. d4 Nc6 15. h3 Nf6 16. Kh2 Qg6 17. Rf1 Re8 18. Be3 d5 19. Nd2 Ne7 20. Kg1 Nf5) 14... Nc6 15. Bb2?? { (0.00 → -4.72) Blunder. b4 was best. } (15. b4 Rf8 16. h3 Nf2 17. Bb2 Qg6 18. c4 Rf3 19. b5 Nd3 20. Rh2 Qg5 21. Na3 Qc5+) 15... Rf8 16. Na3?? { (-4.78 → Mate in 6) Checkmate is now unavoidable. Ba3 was best. } (16. Ba3 Nf2) 16... Qc5+ 17. d4"""
      val sit = Reader.full(pgn).flatMap(_.valid).toOption.get
      Fen.write(sit.state) === EpdFen("5rk1/ppp3pp/2np4/2q5/3Pp1n1/NPP5/PB2Q1PP/R5KR b - d3 0 17")
    }
    "not enpassant in game" in {
      val pgn =
        """1. d4 e6 2. c4 d5 3. Nf3 Nf6 4. Nc3 { D37 Queen's Gambit Declined: Three Knights Variation } Be7 5. e3 O-O 6. cxd5 exd5 7. Bd3 c5 8. h3 cxd4 9. Nxd4 Nc6 10. Nf3 h6 11. O-O Be6?! { (0.09 → 0.64) Inaccuracy. Re8 was best. } (11... Re8 12. a3 Bd6 13. b3 Ne5 14. Bb2 a6 15. Nd4 Nxd3 16. Qxd3 Qe7 17. Nf3 Be6 18. Rfc1) 12. Nb5 a6 13. Nbd4 Qd7 14. Nxe6 fxe6 15. Bd2 e5 16. Be2 e4 17. Nh2 Bd6 18. Ng4 Bc7 19. Bc3 Nxg4 20. Bxg4 Qd6 21. g3 Ne5 22. Kg2? { (0.24 → -1.11) Mistake. Qd4 was best. } (22. Qd4 Rfd8 23. Qxe5 Qxe5 24. Bxe5 Bxe5 25. Be6+ Kf8 26. Rad1 d4 27. exd4 Rxd4 28. Rxd4 Bxd4) 22... g6?? { (-1.11 → 1.77) Blunder. Nd3 was best. } (22... Nd3 23. Be1) 23. f4?? { (1.77 → -3.96) Blunder. Qd4 was best. } (23. Qd4 a5) 23... Nxg4?? { (-3.96 → 2.53) Blunder. exf3+ was best. } (23... exf3+ 24. Bxf3 Nxf3 25. Rxf3 Rxf3 26. Kxf3 Qxg3+ 27. Ke2 Qg2+ 28. Kd3 Qe4+ 29. Kd2 Rf8 30. Qe2) 24. hxg4 Bb6? { (2.54 → 5.08) Mistake. Rf7 was best. } (24... Rf7 25. Rh1) 25. Qd2?? { (5.08 → 1.61) Blunder. Rh1 was best. } (25. Rh1) 25... Rad8 26. Rh1 Kh7?? { (1.41 → 9.98) Blunder. Bxe3 was best. } (26... Bxe3 27. Qxe3) 27. g5?? { (9.98 → 2.26) Blunder. Rxh6+ was best. } (27. Rxh6+ Kg8 28. f5 Kf7 29. Rxg6 Qxg6 30. fxg6+ Kxg6 31. Bd4 Bxd4 32. Qxd4 Rf6 33. Qe5 Rc8) 27... h5"""
      val sit = Reader.full(pgn).flatMap(_.valid).toOption.get
      Fen.write(sit.state) === EpdFen("3r1r2/1p5k/pb1q2p1/3p2Pp/4pP2/2B1P1P1/PP1Q2K1/R6R w - - 0 28")
    }
  }

  "read" should {
    "enpassant" in {
      Fen
        .read(Fen.Epd("1brr2k1/1pq3p1/p6p/P1p1Pp2/2P5/3PRB2/2Q2PPP/R5K1 w - f6 0 28"))
        .get
        .enPassantSquare === Some(Pos.F6)
    }
    "enpassant 2" in {
      Fen
        .read(Fen.Epd("rnbqkbnr/pp1pp1pp/8/2p1Pp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3"))
        .get
        .enPassantSquare === Some(Pos.F6)
    }
  }

  "castling rights with 2 rooks on the same file" >> {
    val fen = Fen.Epd("2bqkb1r/1pp1ppp1/7r/pN2p2p/3PP3/P3P3/1PP1B1PP/R2Q1RK1 w k -")
    Fen.writeOpening(Fen.read(fen).get) === fen
  }
