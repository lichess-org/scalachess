// package chess
// package perft
//
// import org.specs2.specification.core.Fragments
//
// import chess.variant.*
//
// class PerftTest extends ChessTest:
//
//   private def genTests(name: String, tests: List[Perft], variant: Variant, nodeLimit: Long): Fragments =
//     name should:
//       Fragments.foreach(tests) { perft =>
//         perft.id in:
//           val result = perft.withLimit(nodeLimit).calculate(variant)
//           Fragments.foreach(result) { r =>
//             s"${r.depth}" in { r.result must_== r.expected }
//           }
//       }
//
//   val nodeLimits = 1_000_000L
//   genTests("calculate ThreeCheck perfts", Perft.threeCheckPerfts, ThreeCheck, nodeLimits)
//   genTests("calculate Antichess perfts", Perft.antichessPerfts, Antichess, nodeLimits)
//   genTests("calculate Atomic perfts", Perft.atomicPerfts, Atomic, nodeLimits)
//   genTests("calculate Crazyhouse perfts", Perft.crazyhousePerfts, Crazyhouse, nodeLimits)
//   genTests("calculate Horde perfts", Perft.hordePerfts, Horde, nodeLimits)
//   genTests("calculate RacingKings perfts", Perft.racingkingsPerfts, RacingKings, nodeLimits)
//   // for the shake of time we only test the first 50 cases in random.peft, run FullRandomPerftTest.scala for all cases
//   genTests("calculate random perfts", Perft.randomPerfts.take(100), Chess960, nodeLimits)
//   genTests("calculate tricky perfts", Perft.trickyPerfts, Chess960, nodeLimits)
//   genTests("calculate chess960 perfts", Perft.chess960, Chess960, nodeLimits)
