package chess

import chess.eval.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import CoreArbitraries.given

class ScoreTest extends ScalaCheckSuite:

  property("invert is an involution"):
    forAll: (score: Score) =>
      score.invert.invert == score

  property("game over for both sides"):
    forAll: (score: Score) =>
      score.invert.isGameOver == score.isGameOver

  property("winning chances sum to 100%"):
    forAll: (score: Score) =>
      val sum = WinPercent.fromScore(score).value + WinPercent.fromScore(score.invert).value
      Math.abs(sum - 100) < 1e-9

  property("white score from the point of view of either side"):
    forAll: (score: Score, turn: Color) =>
      val white = WhiteScore(score, turn)
      white.pov(turn) == score && white.pov(!turn) == score.invert

  property("white score is the same from both sides"):
    forAll: (score: Score, turn: Color) =>
      WhiteScore(score, turn) == WhiteScore(score.invert, !turn)

  test("export mate"):
    assertEquals(WhiteScore(Score.mated, Color.White).exportMate(Color.White), Some(0))
    assertEquals(WhiteScore(Score.mated, Color.Black).exportMate(Color.Black), Some(0))
    assertEquals(WhiteScore(Score.MateGiven, Color.White).exportMate(Color.White), None)
    assertEquals(WhiteScore(Score.MateGiven, Color.Black).exportMate(Color.Black), None)
    assertEquals(WhiteScore(Score.mate(1), Color.White).exportMate(Color.White), Some(1))
    assertEquals(WhiteScore(Score.mate(1), Color.Black).exportMate(Color.White), Some(-1))
    assertEquals(WhiteScore(Score.mate(1), Color.White).exportMate(Color.Black), Some(1))
    assertEquals(WhiteScore(Score.mate(1), Color.Black).exportMate(Color.Black), Some(-1))
    assertEquals(WhiteScore(Score.cp(-50), Color.Black).exportMate(Color.Black), None)
