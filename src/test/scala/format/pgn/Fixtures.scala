package chess
package format.pgn

object Fixtures {

  val simple = "e3 Nc6 d4 Nf6"

  val raws = List(
    "e3 Nc6 d4 Nf6 c3 e5 dxe5 Nxe5 Bb5 a6 Ba4 b5 Bb3 d5 e4 dxe4 f4 Qxd1+ Kxd1 Nd3 Be3 Ng4 Bd4 Ngf2+ Bxf2 Nxf2+ Ke1 Nxh1 Bd5 Ra7 Bc6+ Kd8 Bxe4 Bd6 g3 Re8 Nd2 f5 Ne2 fxe4 Kf1 e3 Kg2 exd2 Rxh1 Bb7+ Kf2 Bc5+ Kf1 d1=Q#",
    "c4 Nc6 e3 Nf6 h3 Ne4 d3 Nc5 a3 Ne5 d4 d6 dxe5 dxe5 b4 Qxd1+ Kxd1 Ne4 f3 Nf2+ Ke2 Nxh1 Nd2 Ng3+ Ke1 Bf5 Bd3 Bxd3 Rb1 Bxb1 Nxb1 Rd8 Bd2 e6 h4 Be7 Nh3 Bxh4 Nf2 Ke7 Bc3 f6 Nd2 h5 c5 g5 Nc4 Rhg8 Na5 Nh1 Ke2 Nxf2 Be1 Nd3 Nxb7 Bxe1 Nxd8 Rxd8 c6 a5 bxa5 Bxa5 a4 f5 Kd1 Nf4+ Kc2 Rd2+ Kc1 Nxg2 Kb1 Nxe3 Kc1 h4 Kb1 h3 Kc1 h2 Kb1 h1=Q#",
    "d4 Nf6 c4 Nc6 Nc3 e5 Nd5 Nxd5 cxd5 Nxd4 e3 Nf5 e4 Nd4 h4 Qf6 Bg5 Qb6 b3 h6 Bc4 hxg5 h5 Bc5 Ne2 Qa5+ Kf1 d6 Nxd4 Bxd4 Rc1 Qxa2 Rc2 Qa5 Qc1 g4 h6 g3 f3 gxh6 Rxh6 Rxh6 Qxh6 Bf2 Qh8+ Kd7 Qf8 Qe1#",
    "Nc3 c6 Nf3 Na6 b4 Nxb4 Rb1 c5 a3 Nxc2+ Qxc2 b6 Nb5 Ba6 Qa4 Bxb5 Rxb5 Nf6 Bb2 Nd5 Qg4 Nc7 Bxg7 Bxg7 Qxg7 Rf8 Rb3 Ne6 Qxh7 Qb8 Re3 f6 Qg6+ Rf7 g3 Nf8 Qg8 e5 d4 d6 dxc5 Qc7 cxd6 Qc1#",
    "d4 Nf6 Nf3 Nc6 Nbd2 e6 e4 d6 c4 Qe7 Bd3 e5 d5 Nd4 Nxd4 exd4 O-O Bg4 f3 Bd7 Nb3 Qe5 Be2 c5 dxc6 bxc6 Qxd4 Qxd4+ Nxd4 Rb8 b3 Be7 Be3 Bd8 Rfd1 Bb6 Kf2 Ke7 Rd2 Ba5 Rd3 Bb6 Rad1 Rhd8 g4 h6 Bf4 g5 Bg3 h5 h3 h4 Bh2 Rb7 e5 dxe5 Bxe5 Ne8 Kg2 Bc7 Bxc7 Rxc7 Nf5+ Kf8 f4 gxf4 Nxh4 Ng7 Bf3 Ne6 Nf5 Nc5 Rd4 Ne6 Rd6 c5 h4 Ng7 Nxg7 Kxg7 g5 a5 Kf2 Kf8 Bc6 Ke7 Ba4 Bxa4 Rxd8 Bc6 h5 Ke6 h6 Be4 Rh8 Re7 Re1 Kf5 h7 Kg6 Rc8 Kxh7 Rxc5 a4 b4 Kg6 b5 f6 gxf6 Kxf6 b6 a3 Rc7 Rxc7 bxc7 Bb7 Re8 Kf5 c5 Ba6 Ra8 Bb7 Rf8+ Ke5 c6 Ba6 Ra8 Kd6 Rxa6 Kxc7 Kf3 Kb8 Kxf4 Kc8 Ke5 Kc7 Ke6 Kd8 Kd6 Ke8 Ra7 Kf8 c7 Kf7 c8=Q+ Kg6 Qg4+ Kf6 Ra8 Kf7 Qf5+ Kg7 Ra7+ Kg8 Qc8#",
    "e3 Nc6 Nf3 Nf6 Nc3 e6 a3 Bd6 d4 Ng4 h3 Nf6 Bb5 a6 Bxc6 dxc6 e4 Nd7 O-O h5 h4 c5 Bg5 f6 Be3 cxd4 Bxd4 c5 Be3 Qe7 g3 Ne5 Nxe5 Bxe5 Qe2 Bxc3 bxc3 Kf7 Rad1 b5 c4 Rb8 Rd2 Bb7 Rfd1 Bc6 Kg2 Bxe4+ Kf1 Bc6 Bf4 e5 Be3 Qe6 Bxc5 Qh3+ Ke1 Qh1+ Qf1 Qe4+ Re2 Qxc4 Bd6 Rbd8 Bb4 Bf3 Rxd8 Rxd8 Re3 Rd1#",
    "e4 e5 Nf3 Nc6 Nc3 Bb4 Nd5 Nf6 Nxb4 Nxb4 c3 Nc6 Nxe5 Nxe5 d4 Ng6 Bg5 h6 Bxf6 Qxf6 e5 Qe6 Bd3 d6 Qe2 Nf4 Qe4 dxe5 Bb5+ c6 d5 Nxd5 Bc4 O-O O-O b5 Bb3 Bb7 Bc2 Nf4 Qh7#")

  val complete960 = """[Event "Casual game"]
[Site "http://en.lichess.org/analyse/---qxr00"]
[Date "2010.10.30"]
[White "Anonymous"]
[Black "Crafty level 1"]
[WhiteElo "?"]
[BlackElo "?"]
[Result "0-1"]
[PlyCount "42"]
[Variant "Chess960"]
[FEN "rbkrnnbq/pppppppp/8/8/8/8/PPPPPPPP/RBKRNNBQ w KQkq - 0 1"]

1. e3 Nf6 2. Ng3 Ne6 3. Nf3 d5 4. Nd4 Nxd4 5. exd4 e6 6. Re1 Ng4 7. Re2 f6 8. c4 dxc4 9. Be4 Rxd4 10. Bf3 Ne5 11. Ne4 Nxf3 12. gxf3 Bf7 13. Nxf6 gxf6 14. Kd1 e5 15. h4 Bg6 16. Bh2 Bf5 17. Rxe5 fxe5 18. Bxe5 Qxe5 19. Qf1 Qf4 20. d3 Rxd3+ 21. Ke1 Qd2# 0-1"""

  val fromWikipedia = """[Event "F/S Return Match"]
[Site "Belgrade, Serbia Yugoslavia|JUG"]
[Date "1992.11.04"]
[Round "29"]
[White "Fischer, Robert J."]
[Black "Spassky, Boris V."]
[Result "1/2-1/2"]
 
1. e4 e5 2. Nf3 Nc6 3. Bb5 {This opening is called the Ruy Lopez.} 3... a6
4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8  10. d4 Nbd7
11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 15. Nb1 h6 16. Bh4 c5 17. dxe5
Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21. Nc4 Nxc4 22. Bxc4 Nb6
23. Ne5 Rae8 24. Bxf7+ Rxf7 25. Nxf7 Rxe1+ 26. Qxe1 Kxf7 27. Qe3 Qg5 28. Qxg5
hxg5 29. b3 Ke6 30. a3 Kd6 31. axb4 cxb4 32. Ra5 Nd5 33. f3 Bc8 34. Kf2 Bf5
35. Ra7 g6 36. Ra6+ Kc5 37. Ke1 Nf4 38. g3 Nxh3 39. Kd2 Kb5 40. Rd6 Kc5 41. Ra6
Nf2 42. g4 Bd3 43. Re6 1/2-1/2"""

  val inlineComments = """[Event "F/S Return Match"]
[Site "Belgrade, Serbia Yugoslavia|JUG"]
[Date "1992.11.04"]
[Round "29"]
[White "Fischer, Robert J."]
[Black "Spassky, Boris V."]
[Result "1/2-1/2"]
 
1. e4 e5 2. Nf3 Nc6 3. Bb5 {This opening is called the Ruy Lopez.} 3... a6 ; this is an inline comment
4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8  10. d4 Nbd7
11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 15. Nb1 h6 16. Bh4 c5 17. dxe5 ; Openning route to ocupying b6 weak square by Na4-Nb6. This square seemed more important than f5 (Ne2-Ng3-Nf5) because its nearer the black's king.
Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21. Nc4 Nxc4 22. Bxc4 Nb6
23. Ne5 Rae8 24. Bxf7+ Rxf7 25. Nxf7 Rxe1+ 26. Qxe1 Kxf7 27. Qe3 Qg5 28. Qxg5
hxg5 29. b3 Ke6 30. a3 Kd6 31. axb4 cxb4 32. Ra5 Nd5 33. f3 Bc8 34. Kf2 Bf5
35. Ra7 g6 36. Ra6+ Kc5 37. Ke1 Nf4 38. g3 Nxh3 39. Kd2 Kb5 40. Rd6 Kc5 41. Ra6
Nf2 42. g4 Bd3 43. Re6 1/2-1/2"""

val fromChessgames = """[Event "The Match - Braingames World Chess Cham"]
[Site "London"]
[Date "2000.01.04"]
[EventDate "2000.10.12"]
[Round "3"]
[Result "1/2-1/2"]
[White "Garry Kasparov"]
[Black "Vladimir Kramnik"]
[ECO "C67"]
[WhiteElo "2849"]
[BlackElo "2770"]
[PlyCount "106"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 Nf6 4. O-O Nxe4 5. d4 Nd6 6. Bxc6
dxc6 7. dxe5 Nf5 8. Qxd8+ Kxd8 9. Nc3 Bd7 10. b3 h6 11. Bb2
Kc8 12. Rad1 b6 13. Ne2 c5 14. c4 Bc6 15. Nf4 Kb7 16. Nd5 Ne7
17. Rfe1 Rg8 18. Nf4 g5 19. Nh5 Rg6 20. Nf6 Bg7 21. Rd3 Bxf3
22. Rxf3 Bxf6 23. exf6 Nc6 24. Rd3 Rf8 25. Re4 Kc8 26. f4 gxf4
27. Rxf4 Re8 28. Bc3 Re2 29. Rf2 Re4 30. Rh3 a5 31. Rh5 a4
32. bxa4 Rxc4 33. Bd2 Rxa4 34. Rxh6 Rg8 35. Rh7 Rxa2 36. Rxf7
Ne5 37. Rg7 Rf8 38. h3 c4 39. Re7 Nd3 40. f7 Nxf2 41. Re8+ Kd7
42. Rxf8 Ke7 43. Rc8 Kxf7 44. Rxc7+ Ke6 45. Be3 Nd1 46. Bxb6
c3 47. h4 Ra6 48. Bd4 Ra4 49. Bxc3 Nxc3 50. Rxc3 Rxh4 51. Rf3
Rh5 52. Kf2 Rg5 53. Rf8 Ke5 1/2-1/2"""

val disambiguated = "Be5 g6 Ne7g6+"

val fromProd1 = """d4 Nf6 c4 e6 Nc3 Bb4 Bd2 O-O Nf3 c6 e4 Qa5 Be2 Kh8 O-O h5 Ne5 h4 h3 Be7 Nd5 Qd8 Nxe7 d6 Ne7g6+"""
val fromProd2 = """e4 c5 Nf3 Nc6 d4 cxd4 Nxd4 Nf6 Nc3 e5 Nf5 d5 g4 Bxf5 exf5 d4 Ne2 Nxg4 Bh3 Qd5 Ng3 Bb4+ Bd2 Bxd2+ Qxd2 Qf3 Qe2 Qxe2+ Kxe2 Nh6 Bg2 Kf8 Rhe1 Ke7 Kd3 Nb4+ Kd2 Rhc8 c3 dxc3+ bxc3 Rd8+ Kc1 Nd3+ Kc2 Nxe1+ Rxe1 Kf6 Be4 Rdb8 Rd1 Kg5 Rd7 a6 Bxb7 Ra7 Bc6 Rxd7 Bxd7 Rd8 Bc6 Nxf5 Nxf5 Kxf5 f3 Rb8 Be4+ Kf4 c4 Rh8 c5 a5 c6 f5 Bd5 Rc8 Kc3 g6 Kc4 h5 Kb5 Rb8+ Ka6 a4 c7 Rc8 Kb7 Rh8 c8=Q Rxc8 Kxc8 g5 Kd7 g4 fxg4 hxg4 Ke6 e4 Bc4 Kg5 Ke5 e3 Bd3 f4 Ke4 g3 Kf3 gxh2 Kg2 f3+ Kxh2 e2 Bxe2 fxe2 Kh3 e1=Q Kg2 Qe6"""

val promoteRook = """d4 Nf6 f3 d5 e3 e5 Ne2 Bd6 g3 a5 Bg2 Bf5 e4 Bb4+ Nbc3 Bxc3+ Nxc3 exd4 Qxd4 Be6 O-O Nc6 Qf2 Qd6 Bf4 Ne5 Bxe5 Qxe5 Rfd1 Ra6 Rd2 c6 Rad1 Bd7 exd5 b5 d6 Rg8 f4 Qe6 Qc5 Kf8 Bf3 Ke8 Kg2 Kf8 Re2 Qc4 Qxc4 bxc4 h3 Be8 Bg4 h5 Bf5 c5 Red2 g6 Be4 Rb6 Kf3 Kg7 g4 hxg4+ hxg4 Ra6 f5 Ra7 Nd5 g5 Nxf6 Kxf6 b3 Bb5 bxc4 Bxc4 d7 Rxd7 Rxd7 Bxa2 Rd8 Rxd8 Rxd8 a4 Ra8 Ke7 Rxa4 Bb1 c4 Bxe4+ Kxe4 Kd6 f6 Ke6 Ra6+ Kd7 Kf5 Kc7 Kxg5 Kb8 Kh6 Kc7 g5 Kb8 g6 fxg6 Kxg6 Kc7 f7 Kb7 Rd6 Ka7 f8=R Kb7 Rf7+"""

val castleCheck1 = """e3 d5 Be2 Nc6 Nf3 e5 d4 e4 Ne5 Nxe5 dxe5 Qg5 Kd2 d4 g3 dxe3+ fxe3 Bh3 Bh5 O-O-O+"""
val castleCheck2 = """d4 Nf6 c4 e6 Nc3 Bb4 Bd2 O-O#"""

val withNag = """
[Event "Casual Game"]
[Site "?"]
[Date "1851.??.??"]
[Round "?"]
[White "Anderssen, Adolph"]
[Black "Lionel, Kieseritsky"]
[Result "1-0"]
[ECO "C33"]
[WhiteElo "unknown"]
[BlackElo "unknown"]
[Annotator "Hayes, David"]
[PlyCount "45"]

1. e4 {C33: King's Gambit Accepted: 3 Nc3 and 3 Bc4} 1... e5 2. f4 {White offers a pawn to gain better development and control of the center.} 2... exf4 3. Bc4 Qh4+ 4. Kf1 b5?! {Bryan's Counter Gambit. A dubious gambit in modern times, but typical of the attacking style of that time. Here black lures the Bishop from it attacking diagonal against the sensitive f7-pawn, and provides a diagonal for development of his own Bishop to b7 where it will bear down on white's King side. All this value for the price of a pawn.} 5. Bxb5 Nf6 6. Nf3 Qh6 7. d3 Nh5 {The immediate, cheap, and shallow threat of ... Ng3+ is easily defended.} 8. Nh4 {The position is sharp and getting sharper.} 8... Qg5 {Again, playing for cheap threats. In this case, black attacks two pieces at once.} 9. Nf5 c6 {9... g6 10. h4 Qf6 is another complicated position for another day.} 10. g4 {A brilliant move made with a steady hand. Note that white cares little for defensive moves, and is always alert for attack. Now black plays to win the g4-pawn.} 10... Nf6 {Black should have played 10... cxb5 11. gxh5 with a better game.} 11. Rg1 {Now Anderssen sacrifices his Bishop, the first of many sacrifices in this game. White cares little for defensive moves, and plays always for the initiative.} 11... cxb5 12. h4 Qg6 13. h5 {White gets more space.} 13... Qg5 14. Qf3 {White now has the ghastly threat of Bxf4 winning black's Queen next.} 14... Ng8 {Black is forces to clear a path of retreat for his Queen by also retreating one of his only developed pieces.} 15. Bxf4 Qf6 {Black should quickly develop his pieces.} 16. Nc3 Bc5 17. Nd5 {Inviting black to indulge his greed. Also good is 17. d4 Bf8 (17... Bxd4? 18. Nd5 when the Knights savage the board.) 18. Be5.} 17... Qxb2 18. Bd6 Qxa1+ {And why not capture with check!} 19. Ke2 {Now who can resist the tender morsel on g1, but resist he must.} 19... Bxg1 {Black is just too greedy. He has too few pieces developed, and what is developed is sent to the far corners of the board. Now it is white's turn to play. Black may have won after 19... Qb2 (to guard against Nxg7+) 20. Rc1 g6 21. Bxc5 gxf5 (not 21... Qxc1 22. Nd6+ Kd8 23. Nxf7+ Ke8 24. Nc7#).} 20. e5 {Slipping the noose around the neck of the black King.} 20... Na6 {Perhaps 20... Ba6 would have put up more resistance by giving black's King more room to run.} 21. Nxg7+ Kd8 22. Qf6+! {A final pretty sacrifice that ends the game.} 22... Nxf6 {A deflection.} 23. Be7# 1-0
"""

val fromTcec = """[Event "nTCEC - Stage 2 - Season 2"]
[Site "http://www.tcec-chess.net"]
[Date "2013.09.24"]
[Round "5.2"]
[White "Stockfish 160913"]
[Black "Spike 1.4"]
[Result "1-0"]
[Variant "normal"]

1. d4 Nf6 2. c4 e6 3. Nc3 Bb4 4. Qc2 Nc6 5. e3 O-O 6. f4 Bxc3+ 7. bxc3 d6 8. Nf3
e5 9. fxe5 dxe5 10. Be2 Re8 11. O-O Qe7 12. Rb1 b6 13. Rb5 a6 14. Rb2 h6
15. Nh4 Qd6 16. Qd1 Rb8 17. Qd3 b5 18. cxb5 axb5 19. Nf5 Bxf5 20. Qxf5 b4
21. cxb4 Nxb4 22. a3 Nc6 23. dxe5 Nxe5 24. Rd1 Qc5 25. Qc2 Qa7 26. Rxb8 Qxb8
27. h3 c6 28. a4 Qa7 29. Rd4 Nd5 30. Bd2 Rd8 31. a5 c5 32. Re4 Nc6 33. a6 Qb6
34. Be1 Kf8 35. Qa2 Qc7 36. Qa4 Kg8 37. a7 Ra8 38. Bg3 Qc8 39. Bb8 Rxb8
40. axb8=R Qxb8 41. Qb5 Nf6 42. Ra4 Qxb5 43. Bxb5 Ne5 44. Ra8+ Kh7 45. Rc8 Kg6
46. Rxc5 Kf5 47. Ba4 Ke6 48. Bb3+ Kd6 49. Ra5 Nfd7 50. Rd5+ Ke6 51. Kf2 Kf6
52. Rd6+ Ke7 53. Rd4 Nc5 54. Bd5 h5 55. g3 Ne6 56. Ra4 Nc5 57. Ra7+ Kf6 58. Ke2
g6 59. Ra5 Ncd7 60. Kd2 Nf8 61. Ra6+ Ne6 62. h4 Ng4 63. Bxe6 fxe6 64. Kd3 Ne5+
65. Ke4 Ng4 66. Kd4 Ne5 67. Ra8 Nd7 68. Rc8 Ke7 69. e4 Nf6 70. Rc7+ Kd6 71. Rg7
Ng4 1-0"""

val invalidVariant = """[Event "nTCEC - Stage 2 - Season 2"]
[Site "http://www.tcec-chess.net"]
[Date "2013.09.24"]
[Round "5.2"]
[White "Stockfish 160913"]
[Black "Spike 1.4"]
[Result "1-0"]
[Variant "starwars"]

1. d4 Nf6 2. c4 e6 3. Nc3 Bb4 4. Qc2 Nc6 5. e3 O-O 6. f4 Bxc3+ 7. bxc3 d6 8. Nf3
e5 9. fxe5 dxe5 10. Be2 Re8 11. O-O Qe7 12. Rb1 b6 13. Rb5 a6 14. Rb2 h6
15. Nh4 Qd6 16. Qd1 Rb8 17. Qd3 b5 18. cxb5 axb5 19. Nf5 Bxf5 20. Qxf5 b4
21. cxb4 Nxb4 22. a3 Nc6 23. dxe5 Nxe5 24. Rd1 Qc5 25. Qc2 Qa7 26. Rxb8 Qxb8
27. h3 c6 28. a4 Qa7 29. Rd4 Nd5 30. Bd2 Rd8 31. a5 c5 32. Re4 Nc6 33. a6 Qb6
34. Be1 Kf8 35. Qa2 Qc7 36. Qa4 Kg8 37. a7 Ra8 38. Bg3 Qc8 39. Bb8 Rxb8
40. axb8=R Qxb8 41. Qb5 Nf6 42. Ra4 Qxb5 43. Bxb5 Ne5 44. Ra8+ Kh7 45. Rc8 Kg6
46. Rxc5 Kf5 47. Ba4 Ke6 48. Bb3+ Kd6 49. Ra5 Nfd7 50. Rd5+ Ke6 51. Kf2 Kf6
52. Rd6+ Ke7 53. Rd4 Nc5 54. Bd5 h5 55. g3 Ne6 56. Ra4 Nc5 57. Ra7+ Kf6 58. Ke2
g6 59. Ra5 Ncd7 60. Kd2 Nf8 61. Ra6+ Ne6 62. h4 Ng4 63. Bxe6 fxe6 64. Kd3 Ne5+
65. Ke4 Ng4 66. Kd4 Ne5 67. Ra8 Nd7 68. Rc8 Ke7 69. e4 Nf6 70. Rc7+ Kd6 71. Rg7
Ng4 1-0"""

val commentsAndVariations = """
[Event "ICC"]
[Site "Internet Chess Club"]
[Date "2013.09.29"]
[Round "?"]
[White "Pedro"]
[Black "burza"]
[Result "1-0"]
[ECO "B12"]
[WhiteElo "1536"]
[BlackElo "1467"]
[Annotator "Pedro"]
[PlyCount "103"]
[EventDate "2013.??.??"]
[SourceDate "2009.04.28"]
[TimeControl "1800"]

1. e4 d6 2. d4 c6 {I'm out of book right now.} 3. f4 {Black's queenside pawns
seemed a bit weird and not secure to long-castle; therefore I was expecting
short castle for black. Given the fact that black does not have serious piece
development at the moment I decided to go 3.f4 right away. Besides controling
more central squares, it also aims to support e5 advance which might kick out
a knight on natural square f6, making the king more vulnerable. Also it
provides Qe1-Qe3 maneuver which can be good for attack.} Qc7 4. Nf3 h6 $6 {
Losing development time with a profilactic move. The g5 square is not a
serious threat at the moment, specially at this early opening stage with only
one piece developed. Also, h6 might a target for attacking sacrifice.} 5. Bd3 (
{I've considered also} 5. Bc4 {but I was discouraged by} e6) 5... Bg4 6. O-O ; Openning route to ocupying b6 weak square by Na4-Nb6. This square seemed more important than f5 (Ne2-Ng3-Nf5) because its nearer the black's king.
Nd7 {Now I was getting worried the long-castle was comming anyway.
Nevertheless, I have better centre control and prospects of attacking kingside
anyway, specially if I manage to get a open f-file.} 7. Nc3 a6 $6 {I do not
understand why such caution on b5.} 8. Be3 e5 $2 {Breaking the centre too soon,
he must finish his development first, his king is still in the centre} 9. fxe5
dxe5 10. d5 $1 ({Perhaps black was expecting} 10. dxe5 $2 Nxe5 {freeing his
position}) 10... c5 (10... cxd5 $6 11. Nxd5 {occupying central weak square}) (
10... Ngf6 11. dxc6 {eternalizing central weak square or damaging the pawn
structure.}) 11. a4 {Preventing b5, which could cramp my queenside with
further b4 move.} Be7 {Now short-castle seems reasonable} 12. Qe1 g5 $2 {No
development, weakening f5.} 13. Qg3 f6 $4 {Too much worry overprotecting
what's already protected. It's impressive the pressure simple piece play makes,
making him blunder a piece.} 14. Qxg4 Qd6 15. Qh5+ {Stopping his castle.} Kd8
16. Qf7 {Threatning Qg8 taking the rook.} Bf8 17. a5 {Openning route to
ocupying b6 weak square by Na4-Nb6. This square seemed more important than f5 
(Ne2-Ng3-Nf5) because its nearer the black's king.} h5 18. Na4 (18. Nxg5 $5 {
Should have been fun, but I didn't want to risk it because I didn't want to
get low on the clock. I think if it was a 45 45 game for example I would
calculate it further.} fxg5 19. Bxg5+ Ne7 (19... Kc8 $2 20. Qe8+ Kc7 21. Qxa8)
(19... Be7 20. Qg7) 20. Be2 $18) 18... Rb8 (18... Nh6 19. Qxh5) 19. Nb6 $1 {
removing the defender of Bf8} Nxb6 20. axb6 h4 {Black is just cramped, with no
moves.} (20... Qe7 {Impressive how white's queen paralize all black's kingside
pieces, an exchange of queens was called to diffuse that.} 21. Qxe7+ Bxe7) 21.
Nxg5 fxg5 22. Qxf8+ Kd7 {only move} (22... Qxf8 $4 23. Rxf8+ Kd7 24. Rxb8 $18)
23. Qxd6+ (23. Rf7+ {does not work due to} Ne7) 23... Kxd6 24. Rf7 Nh6 25. Rf6+
Ke7 26. Bxg5 Ng4 27. Rf4+ Kd6 28. Rxg4 Rhg8 29. Rf1 Rbf8 30. Rxf8 Rxf8 31. Bxh4
Rf7 32. Rg6+ Kd7 33. Bf2 Rxf2 34. Kxf2 a5 35. Bb5+ Ke7 36. Re6+ Kf7 37. Rxe5
Kf6 38. Rf5+ {Imprecise} ({better was} 38. Re6+ Kf7 39. Bc4 Kg7 40. d6) 38...
Ke7 39. h4 c4 40. Bxc4 Kd6 41. h5 Kc5 42. b3 a4 43. bxa4 Kxc4 44. h6 Kb4 45. h7
Kxa4 46. h8=Q Kb5 47. d6+ Kxb6 48. d7 Kc6 49. d8=Q b5 50. Qd5+ Kb6 51. Qxb5+
Kc7 52. Qhb8# 1-0
"""

val bySmartChess = """
[Event "?"]
[Site "Munich"]
[Date "1979.??.??"]
[Round "?"]
[White "Andersson, U"]
[Black "Robatsch"]
[Result "1-0"]
[Annotator  "Krush, I]

{
http://www.smartchess.com/SmartChessOnline/default.htm
English Double Fianchetto (A30)
When two grandmasters play out moves from a symmetrical opening and begin methodically exchanging pieces, it is almost as if you can hear the collective groan from the audience as they realize a quick draw may soon result.
However, openings such as the one we are about to examine are deceptive in their simplicity. In the hands of a strong technical player such as GM Ulf Andersson of Sweden, the Double Fianchetto Exchange Variation can be slow death for a player of the Black pieces who lets his guard slip for even a moment.
}
1.c4 c5 2.Nf3 Nf6 3.g3 b6 4.Bg2 Bb7 5.0-0 g6 6.b3 Bg7 7.Bb2 0-0 8.Nc3
{
If White strikes first with the advance of his d-pawn, then Black is able to equalize with 8.d4 cxd4 9.Qxd4 Nc6 10.Qd2 d5 11.cxd5 Qxd5, for example 12.Nd4 Ne4! 13.Nxc6 Qxc6 14.Bxg7 Kxg7 15.Qe3 Qc5 16.Qxc5 Nxc5 17.Bxb7 Nxb7 18.Nc3 Rac8 19.Rac1 Rfd8, is completely equal, Barcza-Steiner, Budapest 1948. If we compare this situation to the one that occurs in the line after 8.Nc3 d5, we note that here Black has completed his development and is able to challenge White on the c- and d-files with his rooks.
}
8...d5
{
Black breaks the symmetry of moves and provokes the following flurry of exchanges. In my opinion this move is a little dubious and Black can instead equalize in the variation with 8...Na6!
}
9.Nxd5
{
The only correct way to proceed. If 9.cxd5?! Nxd5 10.Na4 Bxb2 11.Nxb2 Nc6, Black has at least equalized as the White knight on b2 is misplaced.
}
9...Nxd5 10.Bxg7 Kxg7 11.cxd5 Qxd5
{
The exchanging sequence is forcing, as 11...Bxd5 12.d4, is good for White. Note that White wins immediately after 12...cxd4? 13.Qxd4+ Kg8 14.Rfd1, pinning and winning Black's bishop.
}
12.d4 cxd4
{
Black's last opportunity to avoid the endgame that occurs in the game is 12...Na6, when it is probably easier for Black to demonstrate equality.
}
13.Qxd4+
{
Less effective is 13.Nh4 Qd7 14.Bxb7 Qxb7 15.Qxd4+ Kg8 16.Rfd1 Nc6 17.Qd7 (17.Qe4 Rfd8 18.Nf3 0.5-0.5 Sanguinetti-Panno, Buenos Aires 1977) 17...Qxd7 18.Rxd7 Rfd8 0.5-0.5 Tal-Salov, Brussels 1988. In both cases Black equalized.
}
13...Qxd4 14.Nxd4 Bxg2 15.Kxg2
{
An important position in the Double Fianchetto Exchange Variation which in practice has been a deceptively tricky defensive chore for Black.
White has a small nagging initiative for the endgame in that:
a) his knight is ACTIVE and CENTRALIZED, while Black's knight is still at home (indeed, it is not clear where Black should develop his knight);
b) White's rooks are already CONNECTED;
c) White has the maneuver Nd4-b5 in reserve to create pressure against Black's queenside pawns and interfere with Black's development.
}
15...a6
{
Black prevents the possibility of Nc3-b5 and prepares to develop his rook with Ra8-a7 to facilitate doubling of the rooks on the c- or d-file (if allowed). Alternatives include:
15...Nd7, and now:
A) 16.Rfd1 Nf6 17.Nb5! Rfc8 18.Rac1 Rxc1 (if 18...a6? 19.Nc7 Rab8 20.Nxa6 Rxc1 21.Rxc1 Ra8 22.Nb4, and White has won a pawn) 19.Rxc1 a6 20.Nd4 Rd8 21.e3 Nd5 22.Rc6 Rd6 23.Rxd6! exd6 24.Kf1, and White enjoys an advantage in the knight endgame thanks to his better pawn structure, Andersson-Marovic, Banja Luka 1976.
B) 16.Rac1 Rfc8 17.Rfd1, and now:
B1) 17...Nc5 18.b4! Ne4 (18...Na4 19.Nb5! Rxc1 20.Rxc1 a5 21.a3! with a clear advantage for White Smyslov-Benko, Monte Carlo 1968, while worse is 18...Nd7? 19.Ne6+ fxe6 20.Rxc8 Rxc8 21.Rxd7, with a winning endgame for White) 19.Nb5! Rxc1 20.Rxc1 a5 21.Rc4, with an edge for White, Lisitsyn-Levenfish, USSR Ch. 1948.
B2) 17...Kf6 18.Nb5 Rxc1 19.Rxc1 Nc5 20.b4, with an edge for White, Ribli-Sapi, Budapest 1976. These examples show how White's Nd4-b5 idea can make life awkward for Black.
An interesting try is 15...Na6!? 16.Rfd1 Rfc8 17.Rac1, and now:
A) 17...Kf6 18.Nb5 Rxc1 (18...Nc5? - losing a pawn - 19.b4 Ne6 20.Rxc8 Rxc8 21.Nxa7, with a big advantage for White, Smyslov-Castro Rojas, Biel 1976) 19.Rxc1 Nc5 20.b4 Ne6 21.Rc6, reaches a position that occurred in the game Portisch-Pachman, Sarajevo 1963, when White stood better.
B) 17...Nb4 18.Rxc8 (Possible is 18.a3, when White may have a slight pull) 18...Rxc8 19.Rd2 a6 0.5-0.5 Benko-Weinstein, Lone Pine 1975.
Also possible is 15...Rc8, when 16.Rac1 Nd7 17.Rfd1 transposes to variations examined after 15...Nd7.
}
16.Rac1
{
16.Rfc1 Ra7 17.Rc2 (17.e3 Rd7 18.Rc2 e5?! 19.Nf3 f6 20.g4! with advantage to White, Szabo-Bisguier, Gothenburg 1955) 17...Rd8 18.e3 Kf8 (18...e5 19.Nf3 f6 20.Rac1, is good for White) 19.Rac1 Ke8 20.g4! (this idea is thematic - gaining space on the kingside while Black is busy preventing White from penetrating on the queenside) 20...h6 21.h4 Rad7 22.f4 a5 23.Kf3 Rd6 24.h5! with a clear plus for White, Andersson-Hort, Niksic 1978.
}
16...Ra7 17.Rc2 Rd8 18.e3
{
White anchors his centralized knight.
}
18...e5?!
{
A mistake, as White will be able to capitalize on the resultant weakening of the Black pawn structure.
Better is 18...Kf8 19.Rfc1, transposing to the Andersson-Hort game in the previous note, although as we saw, White also enjoys more pleasant prospects in that instance.
}
19.Nf3 f6
{
Passively defending the e-pawn with 19...Re7 can be met with 20.Rfc1 Kf8 21.Rc8, and White stands better.
}
20.g4!
{
White gains space on the kingside and has a perceptible advantage.
}
20...Rd6 21.Rfc1 Nd7 22.Rc6 Rxc6 23.Rxc6
{
An exchange of rooks has exposed Black's tender spots along his third rank (b6, d6).
}
23...Kf7 24.Nd2
{
Andersson redirects his knight to the more lucrative outpost on e4.
}
24...Ke7
{
Keeping the knight out of e4 with 24...f5? allows White to post the knight efficiently elsewhere with 25.gxf5 gxf5 26.Nc4, and Black is losing a pawn, as 26...b5 allows 27.Nd6+ etc.
}
25.Ne4 Rb7 26.b4!
{
Effectively binding Black's position.
}
26...Rb8 27.Nc3
{
Andersson now directs his knight to the more powerful post on d5.
}
27...f5
{
If 27...Rb7 28.Nd5+ Kf7 then 29.e4! leaves Black without any useful move, for example 29...a5 (29...Rb8 30.Rc7, and White wins a pawn after 30...Ke6 31.Ra7 a5 32.bxa5 bxa5 33.Ra6+ Kf7 34.Rxa5, etc., while 29...Kg7 is met by 30.Re6! g5 31.Re7+ Kg6 32.Ne3, and the Black king is practically in a mating net) 30.b5, maintains the bind - Black is running out of moves.
}
28.Nd5+ Kf7 29.Kg3 h5?
{
White's king penetrates easily after this move by Black. But even after 29...fxg4 30.Kxg4, there is a grim defensive task for Black.
}
30.gxf5 gxf5 31.Rd6 Rb7 32.Kh4
{
Andersson introduces his king as the last attacking unit in his endgame attack. Black's kingside pawns have become easy targets.
}
32...Kg7 33.Kxh5  
{
The continuation might be 33...a5 34.b5 Kf7 (34...Kh7? loses to 35.Rxd7+ Rxd7 36.Nf6+) 35.Kg5, and White wins another pawn.
What we learn from a study of a game such as this is that no matter how quiet or simplified a position may appear on the surface, active and accurate defense is paramount as any mistake can lead to a technically lost endgame (with almost certain fatal consequences).
}
1-0
"""

val variations = """
1. e4 d6 2. d4 c6  3. f4  Qc7 4. Nf3 h6 $6  5. Bd3 (  5. Bc4  e6) 5... Bg4 6. O-O Nd7  7. Nc3 a6 $6  8. Be3 e5 $2  9. fxe5 dxe5 10. d5 $1 ( 10. dxe5 $2 Nxe5 ) 10... c5 (10... cxd5 $6 11. Nxd5 ) ( 10... Ngf6 11. dxc6 ) 
"""
}
