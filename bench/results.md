# Benchmark results

## https://github.com/lichess-org/scalachess/commit/422cff9d2d2eee0530cce3fac42345d0b550e803

Using java 25.

    bench / Jmh / run -i 3 -wi 2 -f2

```
                                                   scala 3.7.4                     |    scala 3.8.2             |   diff        |   % diff
Benchmark                                               Score       Error   Units  |                            |
BinaryFenBench.read                                810346.101 ±  5300.596   ops/s  |    879769.886 ±  7817.320  |   69423.785   |   +8.57%
BinaryFenBench.read                                 45904.158 ±  1340.367   ops/s  |     46400.251 ±   699.257  |     496.093   |   +1.08%
BinaryFenBench.read                                   576.976 ±   136.155   ops/s  |       486.981 ±   100.577  |     -89.995   |  -15.60%
BinaryFenBench.write                               675477.892 ± 31261.127   ops/s  |    595897.619 ± 17244.167  |  -79580.273   |  -11.78%
BinaryFenBench.write                                44852.423 ±   363.300   ops/s  |     41812.860 ±  1036.453  |   -3039.563   |   -6.78%
BinaryFenBench.write                                  241.547 ±    12.214   ops/s  |       235.049 ±     4.932  |      -6.498   |   -2.69%
DestinationsBench.antichess                         64907.332 ±   716.382  ops/ms  |     66022.786 ±  7181.532  |    1115.454   |   +1.72%
DestinationsBench.atomic                            26049.514 ±   629.621  ops/ms  |     23641.066 ± 10140.197  |   -2408.448   |   -9.25%
DestinationsBench.chess960                           1747.264 ±    26.080  ops/ms  |      1932.749 ±    82.237  |     185.485   |  +10.62%
DestinationsBench.crazyhouse                        48196.817 ±   388.058  ops/ms  |     50596.402 ±   980.562  |    2399.585   |   +4.98%
DestinationsBench.horde                             64508.266 ±   616.119  ops/ms  |     67696.156 ±  2094.280  |    3187.890   |   +4.94%
DestinationsBench.racingkings                       98268.056 ±   614.094  ops/ms  |    102842.803 ±  3294.318  |    4574.747   |   +4.66%
DestinationsBench.threecheck                        98231.884 ±   894.057  ops/ms  |    103889.537 ±   782.918  |    1657.653   |   +1.69%
DestinationsBench.tricky                             9996.836 ±   111.697  ops/ms  |     10504.385 ±   144.668  |     507.549   |   +5.08%
FenReaderBench.antichess                             1311.609 ±    36.469  ops/ms  |      1284.974 ±    40.871  |     -26.635   |   -2.03%
FenReaderBench.atomic                                 369.221 ±     6.937  ops/ms  |       329.571 ±    11.506  |     -39.650   |  -10.74%
FenReaderBench.chess960                                24.386 ±     0.440  ops/ms  |        23.191 ±     0.279  |      -1.195   |   -4.90%
FenReaderBench.crazyhouse                             535.120 ±     7.128  ops/ms  |       511.324 ±    33.356  |     -23.796   |   -4.45%
FenReaderBench.horde                                  723.781 ±    69.144  ops/ms  |       719.955 ±    14.383  |      -3.826   |   -0.53%
FenReaderBench.racingkings                           1978.995 ±    51.436  ops/ms  |      1922.809 ±    56.012  |     -56.186   |   -2.84%
FenReaderBench.threecheck                             941.052 ±    46.492  ops/ms  |       874.316 ±    64.228  |     -66.736   |   -7.09%
FenReaderBench.tricky                                 142.728 ±     5.845  ops/ms  |       137.908 ±     5.826  |      -4.820   |   -3.38%
FenWriterBench.antichess                              606.649 ±     8.763  ops/ms  |       627.683 ±    18.873  |     21.034    |   +3.47%
FenWriterBench.atomic                                 246.930 ±    41.035  ops/ms  |       224.710 ±    21.779  |    -22.220    |   -8.99%
FenWriterBench.chess960                                18.999 ±     0.345  ops/ms  |        14.699 ±     0.577  |     -4.300    |  -22.63%
FenWriterBench.crazyhouse                             402.911 ±   114.770  ops/ms  |       352.349 ±    29.142  |    -50.562    |  -12.55%
FenWriterBench.horde                                  517.562 ±     4.213  ops/ms  |       529.730 ±    25.448  |     12.168    |   +2.35%
FenWriterBench.racingkings                           1110.315 ±    24.292  ops/ms  |       959.581 ±    17.753  |   -150.734    |  -13.58%
FenWriterBench.threecheck                             885.314 ±    25.085  ops/ms  |       761.574 ±    73.756  |   -123.740    |  -13.98%
FenWriterBench.tricky                                  98.286 ±    20.459  ops/ms  |        84.523 ±     0.958  |    -13.763    |  -14.00%
HashBench.hashes                                     6389.241 ±   647.497   ops/s  |      6972.408 ±   186.834  |    583.167    |   +9.13%
HashBench.repetition3                               26457.957 ±   998.381   ops/s  |     25323.888 ±   547.002  |   -1134.069   |   -4.29%
HashBench.repetition5                               27093.122 ±   293.846   ops/s  |     26636.692 ±   815.328  |    -456.430   |   -1.69%
InsufficientMaterialBench.horde                     30363.252 ±  6865.126   ops/s  |     31005.224 ±  1804.215  |     641.972   |   +2.11%
PerftBench.antichess                                  760.605 ±    28.677   ops/s  |       790.789 ±    20.610  |     30.184    |   +3.97%
PerftBench.antichess                                  270.514 ±    32.982   ops/s  |       289.594 ±    10.579  |     19.080    |   +7.06%
PerftBench.antichess                                   46.618 ±     0.502   ops/s  |        48.335 ±     0.951  |      1.717    |   +3.68%
PerftBench.antichess                                   45.821 ±     0.756   ops/s  |        47.724 ±     2.456  |      1.903    |   +4.15%
PerftBench.atomic                                     329.994 ±    10.972   ops/s  |       321.924 ±    30.300  |     -8.070    |   -2.44%
PerftBench.atomic                                      27.077 ±     0.619   ops/s  |        26.370 ±     0.483  |     -0.707    |   -2.61%
PerftBench.atomic                                       5.574 ±     0.381   ops/s  |         5.568 ±     0.240  |     -0.006    |   -0.11%
PerftBench.atomic                                       0.611 ±     0.056   ops/s  |         0.604 ±     0.060  |     -0.007    |   -1.15%
PerftBench.chess960                                   571.489 ±    50.476   ops/s  |       595.823 ±    23.141  |     24.334    |   +4.26%
PerftBench.chess960                                    44.910 ±     2.196   ops/s  |        46.372 ±     2.616  |      1.462    |   +3.26%
PerftBench.chess960                                     1.601 ±     0.043   ops/s  |         1.615 ±     0.069  |      0.014    |   +0.87%
PerftBench.chess960                                     0.534 ±     0.019   ops/s  |         0.545 ±     0.013  |      0.011    |   +2.06%
PerftBench.crazyhouse                                 925.650 ±    10.270   ops/s  |       960.143 ±    57.193  |     34.493    |   +3.73%
PerftBench.crazyhouse                                  48.573 ±     0.830   ops/s  |        53.815 ±     3.002  |      5.242    |  +10.79%
PerftBench.crazyhouse                                   6.828 ±     0.562   ops/s  |         7.671 ±     0.516  |      0.843    |  +12.35%
PerftBench.crazyhouse                                   2.243 ±     0.238   ops/s  |         2.447 ±     0.094  |      0.204    |   +9.10%
PerftBench.horde                                     1070.508 ±    16.488   ops/s  |      1108.721 ±    88.322  |     38.213    |   +3.57%
PerftBench.horde                                       83.110 ±     0.946   ops/s  |        86.685 ±     4.196  |      3.575    |   +4.30%
PerftBench.horde                                       84.938 ±     2.896   ops/s  |        85.673 ±     0.644  |      0.735    |   +0.87%
PerftBench.horde                                       84.814 ±     2.958   ops/s  |        85.213 ±     3.924  |      0.399    |   +0.47%
PerftBench.racingkings                               1935.384 ±    43.333   ops/s  |      1856.328 ±    37.136  |    -79.056    |   -4.08%
PerftBench.racingkings                                273.342 ±     4.587   ops/s  |       248.293 ±    24.801  |    -25.049    |   -9.17%
PerftBench.racingkings                                 13.409 ±     0.066   ops/s  |        12.711 ±     0.112  |     -0.698    |   -5.21%
PerftBench.racingkings                                 13.603 ±     0.151   ops/s  |        13.130 ±     0.624  |     -0.473    |   -3.48%
PerftBench.threecheck                                2761.000 ±    65.892   ops/s  |      2869.601 ±    71.417  |    108.601    |   +3.93%
PerftBench.threecheck                                  66.802 ±     1.390   ops/s  |        69.795 ±     1.896  |      2.993    |   +4.48%
PerftBench.threecheck                                  66.985 ±     3.014   ops/s  |        70.305 ±     0.426  |      3.320    |   +4.96%
PerftBench.threecheck                                  67.175 ±     1.621   ops/s  |        70.940 ±     1.077  |      3.765    |   +5.60%
PerftBench.tricky                                     329.869 ±    23.810   ops/s  |       348.592 ±    20.549  |     18.723    |   +5.67%
PerftBench.tricky                                      25.610 ±     2.081   ops/s  |        26.162 ±     4.976  |      0.552    |   +2.16%
PerftBench.tricky                                       2.862 ±     0.211   ops/s  |         2.730 ±     0.636  |     -0.132    |   -4.61%
PerftBench.tricky                                       0.342 ±     0.003   ops/s  |         0.354 ±     0.019  |      0.012    |   +3.51%
PgnBench.pgnBuildAndRender                            121.992 ±     0.703   ops/s  |       119.225 ±     5.245  |     -2.767    |   -2.27%
PgnBench.pgnFullParser                                140.410 ±     4.803   ops/s  |       144.369 ±     3.836  |      3.959    |   +2.82%
PgnBench.pgnMainlineParser                            176.606 ±     2.849   ops/s  |       178.696 ±    11.454  |      2.090    |   +1.18%
PgnBench.pgnMainlineWithMetasParser                   164.150 ±     0.640   ops/s  |       166.202 ±     4.168  |      2.052    |   +1.25%
PgnBench.pgnRender                                   1584.604 ±   297.559   ops/s  |      1533.593 ±   132.553  |    -51.011    |   -3.22%
PlayBench.divider                                     350.622 ±     2.406   ops/s  |       342.277 ±     4.491  |     -8.345    |   -2.38%
PlayBench.playMoveOrDropWithPly                       408.951 ±     2.965   ops/s  |       394.822 ±     9.953  |    -14.129    |   -3.45%
TiebreakBench.averageOfOpponentsBuchholz            64450.946 ±   813.803   ops/s  |     47233.537 ±  8970.795  | -17217.409    |  -26.73%
TiebreakBench.averagePerfectPerformanceOfOpponents  64248.643 ±  1452.544   ops/s  |     49330.985 ±  3079.170  | -14917.658    |  -23.22%
TiebreakBench.averagePerformanceOfOpponents         64220.643 ±  1031.982   ops/s  |     49592.163 ±  1276.148  | -14628.480    |  -22.79%
TiebreakBench.averageRatingOfOpponents             103224.183 ±   665.463   ops/s  |     97516.127 ±  1104.264  |  -7708.056    |   -7.47%
TiebreakBench.blackPlayedGames                     141194.917 ±  1334.677   ops/s  |    134165.432 ±  4280.992  |  -7029.485    |   -4.98%
TiebreakBench.blackWonGames                        139355.186 ±  4128.315   ops/s  |    134506.438 ±  2686.120  |  -4848.748    |   -3.48%
TiebreakBench.buchholz                             127545.010 ±   912.316   ops/s  |     74416.169 ±   878.792  | -53128.841    |  -41.67%
TiebreakBench.directEncounter                       30787.761 ±   187.898   ops/s  |     29307.019 ±   976.001  |  -1480.742    |   -4.81%
TiebreakBench.foreBuchholz                         123562.605 ±  4529.902   ops/s  |     75326.550 ±   314.119  | -48236.055    |  -39.04%
TiebreakBench.fullTournament                        11596.043 ±   609.104   ops/s  |     10135.518 ±  2055.843  |  -1460.525    |  -12.59%
TiebreakBench.gamesWon                             140907.458 ±  4057.822   ops/s  |    135081.063 ±  3891.792  |  -5826.395    |   -4.14%
TiebreakBench.koyaSystem                            63050.772 ±   903.951   ops/s  |     59585.371 ±   844.279  |  -3465.401    |   -5.50%
TiebreakBench.perfectTournamentPerformance         297635.005 ±  5884.997   ops/s  |    279501.478 ±  3593.708  | -18133.527    |   -6.09%
TiebreakBench.progressiveScores                    128244.901 ±  2476.655   ops/s  |    100574.404 ±  1205.228  | -27670.497    |  -21.58%
TiebreakBench.sonnebornBerger                      127252.322 ±  2430.142   ops/s  |     73634.331 ±  1573.645  | -53617.991    |  -42.15%
TiebreakBench.tournamentPerformanceRating          294775.809 ± 17396.847   ops/s  |    281094.393 ±  9219.278  | -13681.416    |   -4.64%
[success] Total time: 2794 s (0:46:34.0), completed Feb 26, 2026, 10:24:35 AM
```
