package chess

import Pos._

class DividerPerfTest extends ChessTest {

  val nb = 500
  val iterations = 10
  // val nb = 1
  // val iterations = 1

  val gameReplay = format.pgn.Reader.full("1. e4 c5 2. Nf3 d6 3. Bc4 Nf6 4. d3 g6 5. c3 Bg7 6. Bg5 O-O 7. h3 Nc6 8. Nbd2 a6 9. Bb3 b5 10. Bc2 Bb7 11. O-O Nd7 12. Nh2 f6 13. Be3 e5 14. Ndf3 Ne7 15. Qd2 f5 16. Qe2 h6 17. Bd2 g5 18. g4 f4 19. Bb3+ d5 20. exd5 Bxd5 21. c4 Bf7 22.  cxb5 axb5 23. Bxf7+ Rxf7 24. Bc3 Ng6 25. a3 b4 26. axb4 Rxa1 27. Rxa1 cxb4 28. Bxb4 Qb6 29. Bc3 Re7 30. Ra8+ Kh7 31. Kg2 Nc5 32. Qc2 Qb3 33. Qxb3 Nxb3 34. Rb8 Nc5 35. Rb5 Nxd3 36. Nf1 e4 37. Ng1 Nh4+ 38. Kh2 Bxc3 39. bxc3 Nxf2 40. Rd5 e3 41. Ne2 Nf3+ 42. Kg2 Ne1+ 43. Kh2 f3 44. Neg3 e2 45. Nd2 Ng2 46. Nxf3 e1=Q 47. Nxe1 Nxe1 48. c4 Nc2 49. c5 Ne4 50. c6 Nxg3 51. Kxg3 Re3+ 52. Kf2 Rc3 53. Rd7+ Kg6 54. c7 Nb4 55. Rd6+ Kf7 56. Rxh6 Rxc7 57. Rh7+ Ke6 58. Rxc7 Nd3+ 59. Kf3 Nf4 60. Kg3 Ne2+ 61. Kf2 Nf4 62. Kg3 Ne2+ 63. Kh2 Nf4 64. Rc5 Kf6 65.  Rf5+ Kg6 66. Kg3 Ne2+ 67. Kf3 Nd4+ 68. Ke4 Nxf5 69. gxf5+ Kf6 70. h4 gxh4 71. Kf4 h3 72. Kg3 Kxf5 73. Kxh3 1/2-1/2").err
  def runOne = Divider(gameReplay)
  def run { for (i ← 1 to nb) runOne }

  "playing a game" should {
    "many times" in {
      // runOne.end must beSome.like {
      //   case x => x must beBetween(65, 80)
      // }
      if (nb * iterations > 1) {
        println("warming up")
        run
      }
      println("running tests")
      val durations = for (i ← 1 to iterations) yield {
        val start = System.currentTimeMillis
        run
        val duration = System.currentTimeMillis - start
        println(s"$nb games in $duration ms")
        duration
      }
      val nbGames = iterations * nb
      val moveMicros = (1000 * durations.sum) / nbGames
      println(s"Average = $moveMicros microseconds per game")
      println(s"          ${1000000 / moveMicros} games per second")
      true === true
    }
  }
}
