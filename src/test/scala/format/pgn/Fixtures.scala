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
}
