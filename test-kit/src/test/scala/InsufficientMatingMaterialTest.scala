package chess

import chess.format.EpdFen
import chess.variant.Standard

import InsufficientMatingMaterial.*

class InsufficientMatingMaterialTest extends ChessTest:

  test("bishops on Opposite colors"):

    val trues = List(
      "8/4b3/8/8/8/8/4B3/8 w - - 0 1",
      "8/4b3/8/8/2Q5/1K6/4B3/5B2 w - - 0 1",
      "5b2/1k2b1n1/3q4/8/2Q5/1K6/4B3/5B2 w - - 0 1",
      "6b1/1k3bn1/3q4/8/2Q5/1K6/4bB2/8 w - - 0 1",
      "6b1/1k3bn1/3B4/8/2Q2B2/1K6/4bB2/8 w - - 0 1",
      "2k2b2/5b2/8/8/8/3R4/1K2Q3/5B2 w - - 0 1",
      "2k2b2/6b1/7b/8/8/3R2B1/1K2Q3/5B2 w - - 0 1",
      "2k5/8/8/8/8/3R2B1/1K2Q3/5B2 w - - 0 1"
    ).map(EpdFen(_))

    val falses = List(
      "4b3/8/8/8/8/8/4B3/8 w - - 0 1",
      "5b2/8/8/8/8/3R4/1K2QB2/8 w - - 0 1",
      "8/8/8/8/8/3R4/1K2B3/8 w - - 0 1",
      "5b2/8/8/8/8/3R4/1K2Q3/8 w - - 0 1"
    ).map(EpdFen(_))

    trues.foreach: fen =>
      assert(bishopsOnOppositeColors(fenToGame(fen, Standard).situation.board))

    falses.foreach: fen =>
      assertNot(bishopsOnOppositeColors(fenToGame(fen, Standard).situation.board))

  // Determines whether a color does not have mating material.
  test("apply with board and color"):
    val trues = List(
      "8/6R1/K7/2NNN3/5NN1/4KN2/8/k7 w - - 0 1",
      "8/8/K7/8/1k6/8/8/8 w - - 0 1",
      "7k/8/8/8/3K4/8/8/8 w - - 0 1",
      "7k/5R2/5NQ1/8/3K4/8/8/8 w - - 0 1",
      "krq5/bqqq4/qqr5/1qq5/8/8/8/3qB2K b - -",
      "8/3k4/2q5/8/8/K1N5/8/8 b - -",
      "7k/8/6Q1/8/3K4/8/1n6/8 w - - 0 1"
    ).map(EpdFen(_))

    val falses = List(
      "krq5/bqqq4/qqrp4/1qq5/8/8/8/3qB2K b - - 0 1",
      "8/7B/K7/2b5/1k6/8/8/8 b - -",
      "8/8/K7/2b5/1k6/5N2/8/8 b - - 0 1",
      "7k/5R2/5NQ1/8/3K4/8/1p6/8 w - - 0 1",
      "7k/5R2/5NQ1/8/3K4/8/2n5/8 w - - 0 1",
      "7k/5R2/5NQ1/8/3K4/1r6/8/8 w - - 0 1",
      "7k/8/8/5R2/3K4/8/1n6/8 w - - 0 1",
      "7k/8/6B1/8/3K4/8/1n6/8 w - - 0 1",
      "7k/5P2/8/8/3K4/8/1n6/8 w - - 0 1",
      "7k/6N1/8/8/3K4/8/1n6/8 w - - 0 1"
    ).map(EpdFen(_))

    trues.foreach: fen =>
      val sit = fenToGame(fen, Standard).situation
      assert(apply(sit.board, !sit.color))

    falses.foreach: fen =>
      val sit = fenToGame(fen, Standard).situation
      assertNot(apply(sit.board), !sit.color)
