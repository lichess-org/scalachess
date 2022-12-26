package chess

// import org.specs2.matcher.MustExpectations.akaMust
import org.specs2.matcher.MustMatchers.akaMust

class DT extends ChessTest:

  def makeReplay(moves: String) =
    format.pgn.Reader.full(moves).toOption.get match
      case format.pgn.Reader.Result.Complete(replay) => replay.chronoMoves.map(_.fold(_.before, _.before))
      case x                                         => sys error s"Unexpected incomplete replay $x"

  "the divider finds middlegame and endgame" should {
    "game3" in {
      // http://l.org/v9drYYoa
      val replay = makeReplay(
        "1. e4 c5 2. Nf3 d6 3. d4 cxd4 4. Nxd4 Nc6 5. Nc3 e5 6. Nb3 Nf6 7. f3 Be7 8. Be3 O-O 9. Qd2 b6 10. O-O-O Bb7 11. g4 Rc8 12. h4 a5 13. h5 Nb4 14. g5 Nd7 15. g6 Nc5 16. h6 Nxb3+ 17. axb3 fxg6 18. hxg7 Rxf3 19. Bxb6 Qxb6 20. Qh6 Qe3+ 21. Qxe3 Rxe3 22. Bd3 Nxd3+ 23. cxd3 Bg5 24. Kb1 Kxg7 25. Rh2 Bf4 26. Rh4 h5 27. Rhh1 Rf8 28. Rhg1 g5 29. Rg2 g4 30. Nb5 Rf6 31. Nc7 Rg3 32. Rxg3 Bxg3 33. Ne8+ Kf7 34. Nxf6 Kxf6 35. Rf1+ Kg5 36. Rf7 Ba6 37. Ra7 Bxd3+ 38. Kc1 Bf4+ 39. Kd1 Bxe4 40. Rxa5 g3 41. Ke2 g2 42. Ra1 h4 43. Kf2 h3 44. b4 Kg4 45. b5 h2 46. b6 h1=Q 47. Rg1 Be3+ 48. Kxe3 Qxg1+ 49. Kxe4 Qxb6 50. Kd5 g1=Q 51. Ke6 Qgf2 52. Kd5 Qf3+ 53. Ke6 Qbb3+ 54. Ke7 Qff7+ 55. Kxd6 Qbd5# 0-1"
      )
      val divided = Divider(replay)
      println("Game 3 => " + divided)
      Ply raw divided.middle must beSome { (_: Int) must beBetween(20, 28) }
      Ply raw divided.end must beSome { (_: Int) must beBetween(50, 65) }
    }
  }
