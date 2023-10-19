package chess

class SituationTest extends ChessTest:

  test("detect check: by rook"):
    assert:
      """
  K  r
  """.as(White).check.yes
  test("detect check: by knight"):
    assert:
      ("""
  n
K
""" as White).check.yes
  test("detect check: by bishop"):
    assert:
      ("""
  b


     K
""" as White).check.yes
  test("detect check: by pawn"):
    assert:
      ("""
    p
     K
""" as White).check.yes
  test("detect check: not"):
    assert:
      ("""
   n
K
""" as White).check.no
  test("detect check mate: by rook"):
    assert:
      ("""
PP
K  r
""" as White).checkMate
  test("detect check mate: by knight"):
    assert:
      ("""
PPn
KR
""" as White).checkMate
  test("detect check mate: not"):
    assertNot:
      ("""
  n
K
""" as White).checkMate
  test("stale mate: stuck in a corner"):
    assert:
      ("""
prr
K
""" as White).staleMate
  test("stale mate: not"):
    assertNot:
      ("""
  b
K
""" as White).staleMate

  test("Give the correct winner for a game"):
    val game =
      """
PP
K  r
""" as White
    assert(game.checkMate)
    assertEquals(game.winner, Some(Black))

  test("Not give a winner if the game is still in progress"):
    val game = """
      p
      K
      """ as White
    assertEquals(game.winner, None)

  test("not be playable: with touching kings"):
    val game = "kK BN" as Black
    assertNot(game.playable(true))
    assertNot(game.playable(false))

  test("not be playable: with other side in check"):
    val game = "k Q K" as White
    assertNot(game.playable(true))
    assertNot(game.playable(false))
