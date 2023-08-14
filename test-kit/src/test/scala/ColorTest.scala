package chess

class ColorTest extends munit.FunSuite:

  test("unary !"):
    assertEquals(!White, Black)
    assertEquals(!Black, White)

  test("passablePawnRank"):
    assertEquals(White.passablePawnRank, Rank.Fifth)
    assertEquals(Black.passablePawnRank, Rank.Fourth)
