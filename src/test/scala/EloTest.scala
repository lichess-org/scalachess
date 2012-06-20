package chess

class EloTest extends ChessTest {

  val calc = new EloCalculator
  val calcInflation = new EloCalculator(true)

  def user(e: Int, n: Int) = new {
    val elo = e
    val nbRatedGames = n
  }

  "calculate standard" should {
    import calc._
    "with equal elo" in {
      val (u1, u2) = (user(1400, 56), user(1400, 389))
      "p1 win" in {
        val (nu1, nu2) = calculate(u1, u2, Some(White))
        "new elos" in {
          (nu1, nu2) must_== (1416, 1384)
        }
        "conservation rule" in {
          nu1 - u1.elo + nu2 - u2.elo must_== 0
        }
      }
      "p1 loss" in {
        val (u1, u2) = (user(1400, 56), user(1400, 389))
        val (nu1, nu2) = calculate(u1, u2, Some(Black))
        "new elos" in {
          (nu1, nu2) must_== (1384, 1416)
        }
        "conservation rule" in {
          nu1 - u1.elo + nu2 - u2.elo must_== 0
        }
      }
      "draw" in {
        val (u1, u2) = (user(1400, 56), user(1400, 389))
        val (nu1, nu2) = calculate(u1, u2, None)
        "new elos" in {
          (nu1, nu2) must_== (1400, 1400)
        }
        "conservation rule" in {
          nu1 - u1.elo + nu2 - u2.elo must_== 0
        }
      }
    }
  }
  "calculate standard with inflation" should {
    import calcInflation._
    "with equal elo" in {
      val (u1, u2) = (user(1400, 56), user(1400, 389))
      "p1 win" in {
        val (nu1, nu2) = calculate(u1, u2, Some(White))
        "new elos" in {
          (nu1, nu2) must_== (1417, 1384)
        }
        "conservation rule" in {
          nu1 - u1.elo + nu2 - u2.elo must_== 1
        }
      }
      "p1 loss" in {
        val (u1, u2) = (user(1400, 56), user(1400, 389))
        val (nu1, nu2) = calculate(u1, u2, Some(Black))
        "new elos" in {
          (nu1, nu2) must_== (1384, 1417)
        }
        "conservation rule" in {
          nu1 - u1.elo + nu2 - u2.elo must_== 1
        }
      }
      "draw" in {
        val (u1, u2) = (user(1400, 56), user(1400, 389))
        val (nu1, nu2) = calculate(u1, u2, None)
        "new elos" in {
          (nu1, nu2) must_== (1400, 1400)
        }
        "conservation rule" in {
          nu1 - u1.elo + nu2 - u2.elo must_== 0
        }
      }
    }
    "with greater elo" in {
      val (u1, u2) = (user(1613, 56), user(1388, 389))
      val (nu1, nu2) = calculate(u1, u2, Some(White))
      "new elos" in {
        (nu1, nu2) must_== (1621, 1381)
      }
      "conservation rule" in {
        nu1 - u1.elo + nu2 - u2.elo must_== 1
      }
    }
  }
  "provision" should {
    import calc._
    val (u1, u2) = (user(1613, 8), user(1388, 389))
    val (nu1, nu2) = calculate(u1, u2, Some(White))
    "new elos" in {
      (nu1, nu2) must_== (1628, 1381)
    }
  }
  "no provision" should {
    import calc._
    val (u1, u2) = (user(1313, 1256), user(1158, 124))
    val (nu1, nu2) = calculate(u1, u2, Some(White))
    "new elos" in {
      (nu1, nu2) must_== (1322, 1149)
    }
  }
  "calculate diff" should {
    import calc._
    "same level" in {
      val (u1, u2) = (user(1200, 100), user(1200, 100))
      "win" in {
        diff(u1, u2, Some(White)) must_== 16
      }
      "loss" in {
        diff(u1, u2, Some(Black)) must_== -16
      }
      "draw" in {
        diff(u1, u2, None) must_== 0
      }
    }
    "against higher rated player" in {
      val (u1, u2) = (user(1200, 100), user(1500, 100))
      "win" in {
        diff(u1, u2, Some(White)) must_== 27
      }
      "loss" in {
        diff(u1, u2, Some(Black)) must_== -5
      }
      "draw" in {
        diff(u1, u2, None) must_== 11
      }
    }
  }
}
