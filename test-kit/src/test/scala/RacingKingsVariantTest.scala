package chess

import variant.RacingKings

import chess.format.EpdFen

class RacingKingsVariantTest extends ChessSpecs:

  "Racing Kings" should:

    "disallow discovered check" in:
      val fenPosition = EpdFen("1r6/8/5qRK/8/7Q/8/2knNn2/2b2B2 b - - 11 11")
      val game        = fenToGame(fenPosition, RacingKings).toOption.get
      game.situation.destinations get Square.D2 must beNone

    "game end to black" in:
      val fenPosition = EpdFen("4krn1/K2b4/8/8/8/8/8/8 w - - 4 3")
      val game        = fenToGame(fenPosition, RacingKings).toOption.get
      game.situation.end must beTrue
      game.situation.winner must_==  Option(Black)

    "game end to black 2" in:
      val fenPosition = EpdFen("4brk1/8/5n2/K7/8/8/8/8 w - - 6 4")
      val game        = fenToGame(fenPosition, RacingKings).toOption.get
      game.situation.end must beTrue
      game.situation.winner must_==  Option(Black)

    "game end to black 3" in:
      val fenPosition = EpdFen("3kbrn1/8/8/K7/8/8/8/8 w - - 4 3")
      val game        = fenToGame(fenPosition, RacingKings).toOption.get
      game.situation.end must beTrue
      game.situation.winner must_==  Option(Black)

    "game end to black 4" in:
      val fenPosition = EpdFen("4brk1/4n3/8/K7/8/8/8/8 w - - 4 3")
      val game        = fenToGame(fenPosition, RacingKings).toOption.get
      game.situation.end must beTrue
      game.situation.winner must_==  Option(Black)

    "game end to white" in:
      val fenPosition = EpdFen("K3br2/5k2/8/8/6n1/8/8/8 w - - 4 3")
      val game        = fenToGame(fenPosition, RacingKings).toOption.get
      game.situation.end must beTrue
      game.situation.winner must_==  Option(White)

    "game end to white 2" in:
      val fenPosition = EpdFen("K3b2r/5k2/5n2/8/8/8/8/8 w - - 4 3")
      val game        = fenToGame(fenPosition, RacingKings).toOption.get
      game.situation.end must beTrue
      game.situation.winner must_==  Option(White)

    "game is draw if both kings are in 8th rank" in:
      val fenPosition = EpdFen("K3brk1/8/5n2/8/8/8/8/8 w - - 4 3")
      val game        = fenToGame(fenPosition, RacingKings).toOption.get
      game.situation.end must beTrue
      game.situation.winner must_==  None

    "game is not end when Black's King can go to the 8th rank" in:
      val fenPosition = EpdFen("1K2br2/5k2/5n2/8/8/8/8/8 b - - 3 2")
      val game        = fenToGame(fenPosition, RacingKings).toOption.get
      game.situation.end must beFalse
