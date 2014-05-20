package chess
package format.pgn

object Fixtures {

  val simple = "e3 Nc6 d4 Nf6".split(' ').toList

  val raws = List(
    "e3 Nc6 d4 Nf6 c3 e5 dxe5 Nxe5 Bb5 a6 Ba4 b5 Bb3 d5 e4 dxe4 f4 Qxd1+ Kxd1 Nd3 Be3 Ng4 Bd4 Ngf2+ Bxf2 Nxf2+ Ke1 Nxh1 Bd5 Ra7 Bc6+ Kd8 Bxe4 Bd6 g3 Re8 Nd2 f5 Ne2 fxe4 Kf1 e3 Kg2 exd2 Rxh1 Bb7+ Kf2 Bc5+ Kf1 d1=Q#",
    "c4 Nc6 e3 Nf6 h3 Ne4 d3 Nc5 a3 Ne5 d4 d6 dxe5 dxe5 b4 Qxd1+ Kxd1 Ne4 f3 Nf2+ Ke2 Nxh1 Nd2 Ng3+ Ke1 Bf5 Bd3 Bxd3 Rb1 Bxb1 Nxb1 Rd8 Bd2 e6 h4 Be7 Nh3 Bxh4 Nf2 Ke7 Bc3 f6 Nd2 h5 c5 g5 Nc4 Rhg8 Na5 Nh1 Ke2 Nxf2 Be1 Nd3 Nxb7 Bxe1 Nxd8 Rxd8 c6 a5 bxa5 Bxa5 a4 f5 Kd1 Nf4+ Kc2 Rd2+ Kc1 Nxg2 Kb1 Nxe3 Kc1 h4 Kb1 h3 Kc1 h2 Kb1 h1=Q#",
    "d4 Nf6 c4 Nc6 Nc3 e5 Nd5 Nxd5 cxd5 Nxd4 e3 Nf5 e4 Nd4 h4 Qf6 Bg5 Qb6 b3 h6 Bc4 hxg5 h5 Bc5 Ne2 Qa5+ Kf1 d6 Nxd4 Bxd4 Rc1 Qxa2 Rc2 Qa5 Qc1 g4 h6 g3 f3 gxh6 Rxh6 Rxh6 Qxh6 Bf2 Qh8+ Kd7 Qf8 Qe1#",
    "Nc3 c6 Nf3 Na6 b4 Nxb4 Rb1 c5 a3 Nxc2+ Qxc2 b6 Nb5 Ba6 Qa4 Bxb5 Rxb5 Nf6 Bb2 Nd5 Qg4 Nc7 Bxg7 Bxg7 Qxg7 Rf8 Rb3 Ne6 Qxh7 Qb8 Re3 f6 Qg6+ Rf7 g3 Nf8 Qg8 e5 d4 d6 dxc5 Qc7 cxd6 Qc1#",
    "d4 Nf6 Nf3 Nc6 Nbd2 e6 e4 d6 c4 Qe7 Bd3 e5 d5 Nd4 Nxd4 exd4 O-O Bg4 f3 Bd7 Nb3 Qe5 Be2 c5 dxc6 bxc6 Qxd4 Qxd4+ Nxd4 Rb8 b3 Be7 Be3 Bd8 Rfd1 Bb6 Kf2 Ke7 Rd2 Ba5 Rd3 Bb6 Rad1 Rhd8 g4 h6 Bf4 g5 Bg3 h5 h3 h4 Bh2 Rb7 e5 dxe5 Bxe5 Ne8 Kg2 Bc7 Bxc7 Rxc7 Nf5+ Kf8 f4 gxf4 Nxh4 Ng7 Bf3 Ne6 Nf5 Nc5 Rd4 Ne6 Rd6 c5 h4 Ng7 Nxg7 Kxg7 g5 a5 Kf2 Kf8 Bc6 Ke7 Ba4 Bxa4 Rxd8 Bc6 h5 Ke6 h6 Be4 Rh8 Re7 Re1 Kf5 h7 Kg6 Rc8 Kxh7 Rxc5 a4 b4 Kg6 b5 f6 gxf6 Kxf6 b6 a3 Rc7 Rxc7 bxc7 Bb7 Re8 Kf5 c5 Ba6 Ra8 Bb7 Rf8+ Ke5 c6 Ba6 Ra8 Kd6 Rxa6 Kxc7 Kf3 Kb8 Kxf4 Kc8 Ke5 Kc7 Ke6 Kd8 Kd6 Ke8 Ra7 Kf8 c7 Kf7 c8=Q+ Kg6 Qg4+ Kf6 Ra8 Kf7 Qf5+ Kg7 Ra7+ Kg8 Qc8#",
    "e3 Nc6 Nf3 Nf6 Nc3 e6 a3 Bd6 d4 Ng4 h3 Nf6 Bb5 a6 Bxc6 dxc6 e4 Nd7 O-O h5 h4 c5 Bg5 f6 Be3 cxd4 Bxd4 c5 Be3 Qe7 g3 Ne5 Nxe5 Bxe5 Qe2 Bxc3 bxc3 Kf7 Rad1 b5 c4 Rb8 Rd2 Bb7 Rfd1 Bc6 Kg2 Bxe4+ Kf1 Bc6 Bf4 e5 Be3 Qe6 Bxc5 Qh3+ Ke1 Qh1+ Qf1 Qe4+ Re2 Qxc4 Bd6 Rbd8 Bb4 Bf3 Rxd8 Rxd8 Re3 Rd1#",
    "e4 e5 Nf3 Nc6 Nc3 Bb4 Nd5 Nf6 Nxb4 Nxb4 c3 Nc6 Nxe5 Nxe5 d4 Ng6 Bg5 h6 Bxf6 Qxf6 e5 Qe6 Bd3 d6 Qe2 Nf4 Qe4 dxe5 Bb5+ c6 d5 Nxd5 Bc4 O-O O-O b5 Bb3 Bb7 Bc2 Nf4 Qh7#")

  val noTagButResult = "1.g4 e5 2.d4 e4 3.c4 Qh4 4.h3 Bb4+ 5.Nc3 Bxc3+ 6.bxc3 Qe7 7.Bf4 d6 8.e3 g5 9.Bg3 Be6 10.Rb1 Bc8 11.Be2 Nf6 12.h4 gxh4 13.Bxh4 Qe6 14.g5 Nfd7 15.Nh3 Rg8 16.Nf4 Qe7 17.Nd5 Qd8 18.g6 f6 19.gxh7 1-0"

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

val fromCrafty = """[Event "Live Chess"]
[Site "Chess.com"]
[Date "2014.01.15"]
[Round "?"]
[White "amarkaur"]
[WhiteElo "1116"]
[Black "ludocode"]
[BlackElo "1220"]
[Result "0-1"]
[Annotator "Crafty v23.4"]
{annotating both black and white moves.}
{using a scoring margin of +0.50 pawns.}
{search time limit is 1.00}

  1.     Nf3      d5
  2.      d4     Nc6
  3.      e3     Nf6
  4.      c4    dxc4
  5.    Bxc4     Qd6
                ({15:+0.88}  5. ... Qd6 6. Nc3 Qb4 7. Nd2 e6 8. a3 Qa5 9. O-O Bd6 10. Nde4 Nxe4 11. Nxe4 O-O 12. Bd2 Qf5 13. Nxd6 cxd6 $16)
                ({15:+0.36}  5. ... e6 6. O-O Bd6 7. Nc3 O-O 8. e4 e5 9. d5 Na5 10. Bd3 Bd7 11. Bg5 Re8 12. Be3 Qe7 $14)
  6.     Nc3      e5
  7.     O-O    exd4
  8.    Nxd4
                ({13:+1.22}  8. Nxd4 Ne5 9. Be2 Neg4 10. f4 c5 11. Ndb5 Qe7 12. e4 c4 13. Qa4 Nxe4 14. Nxe4 Qxe4 15. Bxc4 $16)
                ({13:+2.05}  8. exd4 Bg4 9. Nb5 Qd7 10. Bf4 O-O-O 11. Bxc7 Bxf3 12. gxf3 Re8 13. d5 Nb4 14. Rc1 $18)

  8.     ...    Nxd4
  9.    exd4     Be6
                ({13:+1.32}  9. ... Be6 10. d5 Bd7 11. Re1+ Be7 12. Nb5 Bxb5 13. Bxb5+ Kf8 14. Bc4 Rd8 15. Bg5 Qc5 16. Bxf6 Bxf6 $16)
                ({13:+0.70}  9. ... Bd7 10. Re1+ Be7 11. Nb5 Bxb5 12. Bxb5+ c6 13. Bc4 O-O 14. Qb3 b5 15. Bd3 Rfe8 16. Be3 Nd5 $14)
 10.     Re1
                ({13:+0.52}  10. Re1 O-O-O 11. Bxe6+ fxe6 12. Bg5 Rd7 13. Bxf6 gxf6 14. Qb3 Qxd4 15. Rad1 Qb6 16. Qxb6 axb6 17. Rxe6 Rxd1+ 18. Nxd1 $14)
                ({13:+1.32}  10. d5 Bd7 11. Nb5 Qb6 12. Re1+ Kd8 13. Qd3 Bc5 14. Be3 Bxe3 15. Rxe3 Re8 16. Ree1 Kc8 17. Rxe8+ Bxe8 $16)

 10.     ...   O-O-O
 11.     Be2
                ({15:-0.76}  11. Be2 Qxd4 12. Qxd4 Rxd4 13. Be3 Rb4 14. Rab1 Bf5 15. a3 Bxb1 16. axb4 Bf5 17. b5 Kb8 18. Rd1 $17)
                ({15:+0.49}  11. Bxe6+ fxe6 12. Bg5 Qxd4 13. Qxd4 Rxd4 14. Rxe6 Rd6 15. Re5 h6 16. Bxf6 Rxf6 17. Re8+ Kd7 18. Rae1 Rd6 $14)

 11.     ...    Qxd4
 12.     Be3     Qb4
 13.     Qc1     Bd6
 14.      a3     Qh4
 15.      g3     Qh3
 16.     Bf3
                ({14:-2.01}  16. Bf3 Ng4 17. Bxg4 Bxg4 18. f4 a6 19. Qc2 Rhe8 20. Ne4 Bf5 21. Nxd6+ Rxd6 22. Qg2 Qg4 $19)
                ({14:+0.01}  16. Bxa7 Bd7 17. Bf1 Qf5 18. Qd1 Bc6 19. Bd3 Qh3 20. Bf1 Qf5 $10)

 16.     ...     Ng4
 17.    Bxg4    Bxg4
 18.      f3
                ({15:-5.02}  18. f3 Bxf3 19. Re2 Bxe2 20. Nxe2 Rhe8 21. Nf4 Qg4 22. Ng2 b6 23. Qc2 Be5 24. Rc1 Rd5 25. Qc6 $19)
                ({15:-1.82}  18. f4 f5 19. Nb5 a6 20. Nxd6+ Rxd6 21. Qc2 Bf3 22. Rac1 Bc6 23. Bc5 Rh6 24. Re7 Be4 25. Qd2 $19)

 18.     ...    Bxg3
 19.    hxg3
                ({14:-Mat06}  19. hxg3 Qxg3+ 20. Kf1 Qxf3+ 21. Kg1 Qg3+ 22. Kf1 Bh3+ 23. Ke2 Qg4+ 24. Kf2 Qg2# $19)
                ({14:-6.51}  19. Re2 Bxf3 20. hxg3 Qxg3+ 21. Kf1 Bg4 22. Rd2 Rxd2 23. Qxd2 Bh3+ 24. Ke2 Qg2+ 25. Kd1 Rd8 26. Bd4 Qf1+ 27. Qe1 Rxd4+ 28. Kc2 Bf5+ 29. Kb3 $19)

 19.     ...   Qxg3+
 20.     Kf1    Bh3+
                ({9:-9.33}  20. ... Bh3+ 21. Ke2 Qh2+ 22. Bf2 Rhe8+ 23. Qe3 Rxe3+ 24. Kxe3 Qe5+ 25. Ne4 f5 26. Bh4 f4+ 27. Kf2 Qd4+ 28. Ke2 Qxb2+ $19)
                ({9:-Mat05}  20. ... Qxf3+ 21. Kg1 Qg3+ 22. Kf1 Bh3+ 23. Ke2 Qg4+ 24. Kf2 Qg2# $19)
 21.     Ke2    Qg2+
 22.     Bf2   Rhe8+
 23.     Ne4     Bg4
                ({13:-9.33}  23. ... Bg4 24. Qf4 Bxf3+ 25. Qxf3 Rxe4+ 26. Qxe4 Qxe4+ 27. Kf1 Qh1+ 28. Ke2 Qh5+ 29. Kf1 Rd2 30. Kg1 Qg4+ 31. Kf1 Rxb2 32. Re7 $19)
                ({13:-Mat07}  23. ... Rxe4+ 24. Qe3 Rxe3+ 25. Kxe3 Qg5+ 26. f4 Qc5+ 27. Ke2 Qh5+ 28. Ke3 Bg2 29. Bh4 Qf3# $19)
 24.     Qe3      f5
                ({15:-4.06}  24. ... f5 25. Rg1 Bxf3+ 26. Ke1 Bxe4 27. Rxg2 Bxg2 28. Rd1 Rxe3+ 29. Bxe3 Rxd1+ 30. Kxd1 g6 31. Kd2 a5 $19)
                ({15:-11.88}  24. ... Bxf3+ 25. Qxf3 Rxe4+ 26. Qxe4 Qxe4+ 27. Kf1 Qh1+ 28. Ke2 Qh5+ 29. Kf1 Qh3+ 30. Kg1 Rd6 31. Re8+ Kd7 32. Re3 Rg6+ 33. Bg3 Rxg3+ 34. Rxg3 Qxg3+ 35. Kh1 Qf3+ 36. Kg1 Qe3+ 37. Kh1 Qe4+ 38. Kh2 $19)
 25.     Rg1   Bxf3+
 26.    Qxf3
                ({15:-18.62}  26. Qxf3 Rxe4+ 27. Qxe4 Qxe4+ 28. Kf1 Qd3+ 29. Kg2 Rd6 30. Rge1 Rg6+ 31. Kh2 Qf3 32. Re8+ Kd7 33. Re7+ Kxe7 34. Re1+ Kf7 35. Re7+ Kxe7 36. Bc5+ Ke6 37. Bxa7 $19)
                ({15:-4.05}  26. Ke1 Bxe4 27. Rxg2 Bxg2 28. Qxe8 Rxe8+ 29. Kd2 g6 30. Rg1 Be4 31. Be3 b6 32. Bf4 Rd8+ 33. Ke2 c5 34. Rg5 $19)

 26.     ...   Rxe4+
 27.    Qxe4   Qxe4+
 28.     Kf1    Qd3+
 29.     Kg2     Qb3
                ({13:-9.38}  29. ... Qb3 30. Rge1 Rd2 31. Re8+ Kd7 32. Rae1 Qxb2 33. R8e7+ Kd6 34. R1e6+ Kd5 35. Re5+ Qxe5 36. Rd7+ Qd6 37. Rxd6+ Kxd6 $19)
                ({13:-22.79}  29. ... Rd6 30. Rge1 Rg6+ 31. Kh2 Qf3 32. Re8+ Kd7 33. Rd8+ Kxd8 34. Rd1+ Qxd1 35. Bh4+ Kd7 36. Bg3 Qe2+ 37. Kh3 Rh6+ 38. Bh4 Qe3+ 39. Kg2 Rxh4 $19)
 30.    Rgc1
                ({12:-9.97}  30. Rgc1 Rd2 31. Rf1 Rxb2 32. Kg1 Qf3 33. Rae1 Qg4+ 34. Kh2 c5 35. Re3 Kc7 36. Kh1 Qh5+ 37. Kg2 $19)
                ({12:-9.38}  30. Rae1 Rd2 31. Re8+ Kd7 32. Rge1 Qxb2 33. R8e7+ Kd6 34. R1e6+ Kd5 35. Re5+ Qxe5 36. Rd7+ Qd6 37. Rxd6+ Kxd6 $19)

 30.     ...    Qxb2
                ({13:-9.47}  30. ... Qxb2 31. Rd1 Rxd1 32. Rxd1 Qxa3 33. Bh4 Qa2+ 34. Kg1 c5 35. Re1 Qa4 36. Bg3 Qd4+ 37. Kg2 Kd7 $19)
                ({13:-12.77}  30. ... Rd2 31. Rh1 Qc2 32. Rhf1 Qe4+ 33. Kh2 Qh4+ 34. Kg2 Qg4+ 35. Kh1 Qh3+ 36. Kg1 Rd6 37. Bh4 Rg6+ 38. Kf2 Rg2+ 39. Ke1 Qxh4+ 40. Kd1 $19)
 31.     Kg1
                ({14:-15.11}  31. Kg1 Rd6 32. Re1 Rg6+ 33. Kf1 Qb5+ 34. Re2 Qd5 35. Re8+ Kd7 36. Re3 Qh1+ 37. Ke2 Qxa1 38. Rd3+ Kc6 39. Bxa7 Rg2+ 40. Bf2 Qa2+ 41. Rd2 Qxa3 $19)
                ({14:-9.45}  31. Rd1 Rxd1 32. Rxd1 Qxa3 33. Rh1 Qa6 34. Rh4 c5 35. Kf3 Qd3+ 36. Be3 Qd5+ 37. Kg3 h6 38. Ra4 Qb3 $19)

 31.     ...     Rd2
                ({13:-10.53}  31. ... Rd2 32. Rcb1 Qc2 33. Rf1 Qa4 34. Rae1 Qg4+ 35. Kh2 f4 36. Kh1 Kd7 37. a4 Qh5+ 38. Kg2 Qd5+ 39. Kh2 Qh5+ 40. Kg2 $19)
                ({13:-15.11}  31. ... Rd6 32. Re1 Rg6+ 33. Kf1 Qb5+ 34. Re2 Qd5 35. Re8+ Kd7 36. Re3 Qh1+ 37. Ke2 Qxa1 38. Rd3+ Kc6 39. Bxa7 Rg2+ 40. Bf2 Qa2+ 41. Rd2 Qxa3 $19)
 32.    Rab1     Qf6
                ({15:-12.64}  32. ... Qf6 33. Rb3 Qg5+ 34. Rg3 Qf4 35. Rg2 Rxf2 36. Rxc7+ Kxc7 37. Rxf2 Qg3+ 38. Rg2 Qe1+ 39. Kh2 g5 40. Rg3 Qe2+ 41. Rg2 Qe5+ 42. Rg3 $19)
                ({15:-13.99}  32. ... Qxa3 33. Rd1 Ra2 34. Rd4 Rxf2 35. Kxf2 Qa2+ 36. Ke3 Qxb1 37. Kd2 Qb2+ 38. Kd3 Qb3+ 39. Kd2 g5 40. Rd3 Qb2+ 41. Ke3 g4 $19)
 33.     Bg3
                ({14:-Mat04}  33. Bg3 Qd4+ 34. Kf1 Qd3+ 35. Kg1 Qxg3+ 36. Kh1 Rh2# $19)
                ({14:-12.64}  33. Rb3 Qg5+ 34. Rg3 Qf4 35. Rg2 Rxf2 36. Rxc7+ Kxc7 37. Rxf2 Qg3+ 38. Rg2 Qe1+ 39. Kh2 g5 40. Rg3 Qe2+ 41. Rg2 Qe5+ 42. Rg3 $19)

 33.     ...     Qg5
                ({7:-10.72}  33. ... Qg5 34. Rxc7+ Kd8 35. Rc3 f4 36. Rxb7 fxg3 37. Rxa7 $19)
                ({7:-Mat04}  33. ... Qd4+ 34. Kf1 Qd3+ 35. Kg1 Qxg3+ 36. Kh1 Rh2# $19)
 34.   Rxc7+     Kd8

       0-1""" 

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

val fromLichessBadPromotion = """
[Event "?"]
[Site "?"]
[Date "????.??.??"]
[Round "?"]
[White "?"]
[Black "?"]
[Result "*"]
[FEN "8/8/1KP5/3r4/8/8/8/k7 w - - 0 1"]
[SetUp "1"]

1. c7 Rd6+ 2. Kb5 Rd5+ 3. Kb4 Rd4+ 4. Kb3 Rd3+ 5. Kc2 Rd4 6. c8=R Ra4 7. Kb3 *
"""

val fromTcecWithEngineOutput = """
[Event "nTCEC - Stage 2 - Season 2"]
[Site "http://www.tcec-chess.net"]
[Date "2013.10.09"]
[Round "13.9"]
[White "Komodo 1092"]
[Black "Stockfish 160913"]
[Result "1-0"]
[WhiteElo "3113"]
[BlackElo "3110"]
[Time "05:39:46"]
[ECO "D10"]
[Opening "Slav: 3.Nc3 Nf6 4.e3"]
[TimeControl "7200+30"]
[PlyCount "165"]
[Number "117"]
[Termination "GUI adjudication"]
[WhiteType "program"]
[BlackType "program"]
[Variant "normal"]

{ 2 x Intel Xeon E5-2689 }
1.d4 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=d4, tb=0, R50=50, wv=0.00,  }  
d5 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=d5, tb=0, R50=50, wv=0.00,  }  
2.c4 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=c4, tb=0, R50=50, wv=0.00,  }  
c6 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=c6, tb=0, R50=50, wv=0.00,  }  
3.Nc3 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=Nc3, tb=0, R50=49, wv=0.00,  }  
Nf6 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=Nf6, tb=0, R50=49, wv=0.00,  }  
4.e3 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=e3, tb=0, R50=50, wv=0.00,  }  
g6 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=g6, tb=0, R50=50, wv=0.00,  }  
5.Nf3 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=Nf3, tb=0, R50=49, wv=0.00,  }  
Bg7 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=Bg7, tb=0, R50=49, wv=0.00,  }  
6.Be2 { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=Be2, tb=0, R50=48, wv=0.00,  }  
O-O { ev=0.00, d=1, mt=00:00:00, tl=02:00:30, s=0 kN/s, n=0, pv=O-O, tb=0, R50=48, wv=0.00,  }  
7.O-O { ev=0.26, d=22, pd=Be6, mt=00:01:39, tl=01:58:50, s=8705 kN/s, n=867251059, pv=O-O Be6 b3 a6 Bd2 Bf5 h3 Nbd7 Rc1 Re8 g4 Be6 Ng5 Nf8 f4 h6 Nxe6 Nxe6 cxd5 cxd5 h4 Qd6 g5 Ne4 Nxe4, tb=0, R50=47, wv=0.26,  }  
Bg4 { ev=-0.44, d=32, pd=Qb3, mt=00:02:29, tl=01:58:00, s=17364 kN/s, n=2599449979, pv=Bg4 Qb3 Qb6 Bd2 e6 h3 Bxf3 Bxf3 Nbd7 Rfd1 Rfe8 Be2 dxc4 Bxc4 Qxb3 axb3 Red8 g4 Nb6 Kg2 Nfd5 Nxd5 Nxd5 Ba5 Nb6, tb=0, R50=47, wv=0.44,  }  
8.Qb3 { ev=0.36, d=25, pd=Qb6, mt=00:02:59, tl=01:56:21, s=15490 kN/s, n=2790759992, pv=Qb3 Qb6 h3 Bxf3 Bxf3 dxc4 Qxc4 Nbd7 b4 e5 a4 a6 Rd1 Qc7 a5 Ne8 Qb3 Rd8 d5 e4 Be2 Ne5 Bb2 Nf6 Rac1, tb=0, R50=46, wv=0.36,  }  
Qb6 { ev=-0.48, d=31, pd=h3, mt=00:01:26, tl=01:57:05, s=18266 kN/s, n=1576985656, pv=Qb6 h3 Bxf3 Bxf3 e6 Qa4 Qa6 Qxa6 Nxa6 Rb1 Nc7 b3 Rfe8 Bb2 Bf8 Rfc1 Kg7 g3 Bb4 Kg2 Rad8 a3 Be7 c5 Nd7, tb=0, R50=46, wv=0.48,  }  
9.h3 { ev=0.34, d=25, pd=Bxf3, mt=00:02:01, tl=01:54:50, s=17533 kN/s, n=2135695329, pv=h3 Bxf3 Bxf3 e6 Bd2 Nbd7 Rfd1 Rfd8 Be2 Qc7 Rac1 Nb6 cxd5 exd5 Bd3 a5 a4 Nc8 f3 Qe7 Ne2 Nd6 Qc3 Bh6 Nf4, tb=0, R50=50, wv=0.34,  }  
Bxf3 { ev=-0.38, d=34, pd=Bxf3, mt=00:01:20, tl=01:56:15, s=17740 kN/s, n=1422282657, pv=Bxf3 Bxf3 e6 Rd1 Nbd7 Bd2 Rfd8 Be2 dxc4 Bxc4 Qxb3 axb3 Nb6 Be2 Nbd5 g4 Bf8 h4 Be7 g5 Nxc3 bxc3 Ne4 Be1 Nd6, tb=0, R50=50, wv=0.38,  }  
10.Bxf3 { ev=0.25, d=24, pd=e6, mt=00:00:57, tl=01:54:24, s=17221 kN/s, n=992438519, pv=Bxf3 e6 Na4 Qc7 cxd5 exd5 Nc5 b6 Na4 Re8 Nc3 Na6 Bd2 Qd6 Rfc1 Nc7 Rc2 Ne6 Rac1 Rad8 Be2 b5 Bd3 Ng5 a4, tb=0, R50=50, wv=0.25,  }  
e6 { ev=-0.38, d=33, pd=Rd1, mt=00:02:52, tl=01:53:54, s=18817 kN/s, n=3240609540, pv=e6 Rd1 Nbd7 Bd2 Rfb8 Be2 dxc4 Bxc4 Qxb3 axb3 Rd8 g4 Nd5 Be2 Bf6 Kg2 Kg7 Ne4 Be7 g5 h6 h4 hxg5 hxg5 a6, tb=0, R50=50, wv=0.38,  }  
11.Bd2 { ev=0.25, d=25, pd=Nbd7, mt=00:04:36, tl=01:50:18, s=14418 kN/s, n=3989679346, pv=Bd2 Nbd7 Rfd1 Rfd8 Rac1 Rac8 Be2 dxc4 Bxc4 Qxb3 axb3 Nb6 Be2 Nbd5 Ra1 a6 Bf3 Nd7 Ra5 Ra8 Ra4 N5b6 Raa1 f5 Ne2, tb=0, R50=49, wv=0.25,  }  
Nbd7 { ev=-0.42, d=34, pd=Rfd1, mt=00:01:16, tl=01:53:08, s=17892 kN/s, n=1360474566, pv=Nbd7 Rfd1 Rfb8 Be2 dxc4 Bxc4 Qxb3 axb3 Rd8 g4 Nd5 Kg2 Bf6 Ne4 Be7 g5 Kg7 h4 a6 Rac1 Rac8 Be2 Rf8 Bf3 f5, tb=0, R50=49, wv=0.42,  }  
12.Rfd1 { ev=0.26, d=25, pd=Rfd8, mt=00:02:26, tl=01:48:23, s=16901 kN/s, n=2486621695, pv=Rfd1 Rfd8 Rac1 Rac8 Be2 dxc4 Bxc4 Qxb3 axb3 Nb6 Be2 Bf8 Bf3 Nfd5 g3 Nb4 Be2 f5 Kg2 Ra8 Na4 N6d5 Nc5 Bxc5 dxc5, tb=0, R50=48, wv=0.26,  }  
Rfb8 { ev=-0.40, d=33, pd=Rab1, mt=00:07:49, tl=01:45:49, s=19416 kN/s, n=9120778899, pv=Rfb8 Rab1 Qxb3 axb3 Nb6 Be2 Re8 g4 e5 dxe5 Rxe5 Kg2 Ne4 Nxe4 dxe4 Bb4 Ree8 Bd6 Be5 Bxe5 Rxe5 h4 Kg7 Ra1 a6, tb=0, R50=48, wv=0.40,  }  
13.Be2 { ev=0.31, d=25, pd=Qc7, mt=00:04:42, tl=01:44:11, s=8437 kN/s, n=2390228202, pv=Be2 Qc7 Qc2 Rd8 Rac1 dxc4 Bxc4 Nb6 Bd3 Qd6 a3 e5 Ne2 exd4 Nxd4 Nfd5 Nf3 Rd7 h4 Re8 h5 Red8 Be2 Re8 hxg6, tb=0, R50=47, wv=0.31,  }  
dxc4 { ev=-0.38, d=32, pd=Bxc4, mt=00:02:11, tl=01:44:08, s=20023 kN/s, n=2625161804, pv=dxc4 Bxc4 Qxb3 axb3 Rd8 Be2 Nd5 g4 Bf6 Ne4 Be7 g5 Kg7 h4 h5 Kg2 a6 Ba5 Rdc8 Bc3 Re8 Bd3 Rad8 Ba5 Rc8, tb=0, R50=50, wv=0.38,  }  
14.Bxc4 { ev=0.39, d=23, pd=Qxb3, mt=00:01:41, tl=01:43:00, s=17663 kN/s, n=1803476132, pv=Bxc4 Qxb3 axb3 Rd8 g4 Nb6 Be2 Nbd5 Kg2 Ne8 Nxd5 exd5 Ba5 Rd7 h4 Nd6 Bb6 a6 h5 Bf6 Bd3 Re8 Rh1 Ne4 b4, tb=0, R50=50, wv=0.39,  }  
Qxb3 { ev=-0.44, d=33, pd=axb3, mt=00:09:14, tl=01:35:24, s=20337 kN/s, n=11276634044, pv=Qxb3 axb3 Rd8 e4 b5 Bd3 Nc5 Bxb5 Rxd4 Be3 Rxd1 Rxd1 cxb5 Bxc5 Rc8 b4 a6 e5 Ne8 f4 f6 exf6 Bxf6 Kf2 Rd8, tb=0, R50=50, wv=0.44,  }  
15.axb3 { ev=0.33, d=26, pd=Rd8, mt=00:03:31, tl=01:39:59, s=15744 kN/s, n=3341606297, pv=axb3 Rd8 g4 a6 g5 Nd5 Ne4 Bf8 Kg2 h5 h4 Be7 Be2 Kg7 Ba5 Rh8 Bc3 Rhf8 Rg1 Rac8 Bd3 Rb8 Ba5 Bb4 Rgd1, tb=0, R50=50, wv=0.33,  }  
Ne8 { ev=-0.44, d=31, pd=g4, mt=00:01:46, tl=01:34:08, s=13322 kN/s, n=2151364497, pv=Ne8 g4 Nd6 Be2 Nb6 f3 Rc8 Kg2 a6 Ne4 Nxe4 fxe4 Bf6 Kg3 Bd8 Bc3 Bc7 Kg2 Bd8 Kf3 Nd7 Kg3 Bg5 d5 cxd5, tb=0, R50=49, wv=0.44,  }  
16.Be2 { ev=0.47, d=25, pd=Nc7, mt=00:03:29, tl=01:37:01, s=18493 kN/s, n=3871138292, pv=Be2 Nc7 g3 Nd5 Kg2 Re8 e4 Nb4 Rac1 a5 Bf4 Nf6 Bg5 h6 Bd2 Red8 Bf4 Nd7 Bc7 Rdc8 Bd6 Bf8 Bf4 Kg7 e5, tb=0, R50=49, wv=0.47,  }  
a6 { ev=-0.38, d=32, pd=g3, mt=00:04:06, tl=01:30:32, s=20400 kN/s, n=5029048486, pv=a6 g3 f5 Kg2 Nd6 Na4 Kf7 f3 Re8 Ba5 Nf6 Nb6 Nd5 Kf2 Bh6 Rd3 Nxb6 Bxb6 Nb5 Rdd1 Bf8 Ba5 Be7 h4 Bd6, tb=0, R50=50, wv=0.38,  }  
17.Na4 { ev=0.47, d=24, pd=a5, mt=00:03:26, tl=01:34:05, s=18245 kN/s, n=3769943507, pv=Na4 a5 g3 Nc7 Kg2 Nd5 Nc3 Nb4 Ne4 Bf8 Bc3 Be7 Rdc1 Rd8 Nd2 Bd6 e4 Bc7 Ra3 Bb6 Nc4 Bc7 e5 h5 Nd2, tb=0, R50=49, wv=0.47,  }  
f5 { ev=-0.30, d=32, pd=g3, mt=00:01:58, tl=01:29:04, s=19974 kN/s, n=2375071266, pv=f5 g3 Kf7 Be1 Nef6 b4 Nd5 Nc5 N7f6 Bd3 Ne8 Bd2 Nd6 f3 Re8 Kg2 Nc7 Rdc1 Rad8 Nxb7 Nxb7 Rxc6 Re7 Rac1 Rdd7, tb=0, R50=50, wv=0.30,  }  
18.b4 { ev=0.41, d=25, pd=Kf7, mt=00:04:42, tl=01:29:54, s=13034 kN/s, n=3672809064, pv=b4 Kf7 Nc5 Nb6 g3 Nd6 f3 Re8 Bc1 Re7 e4 h6 Bf4 Rd8 b3 Nb5 Bxb5 axb5 Be5 Kg8 Kg2 fxe4 fxe4 Nc8 Nd3, tb=0, R50=50, wv=0.41,  }  
Kf7 { ev=-0.30, d=34, pd=Bd3, mt=00:03:46, tl=01:25:48, s=20741 kN/s, n=4699142749, pv=Kf7 Bd3 Nd6 g3 Re8 Nc5 Nb6 Bc2 Rab8 Bb3 Ne4 Nxe4 fxe4 Kg2 Nd5 f3 Bh6 f4 Bf8 Kf2 Nxb4 Bxb4 Bxb4 Bc2 Kf6, tb=0, R50=49, wv=0.30,  }  
19.Nc5 { ev=0.47, d=24, pd=Nb6, mt=00:04:59, tl=01:25:25, s=19256 kN/s, n=5784859753, pv=Nc5 Nb6, tb=0, R50=49, wv=0.47,  }  
Nb6 { ev=-0.42, d=33, pd=g3, mt=00:01:19, tl=01:25:00, s=21132 kN/s, n=1680669218, pv=Nb6 g3 Nd6 Bd3 Nd5 f3 Re8 Kg2 Nb6 Rac1 Rab8 Re1 Bh6 Kf2 Bg7 Bc2 Bf6 Bc3 Nb5 Kg2 Bg5 h4 Bh6 Rcd1 Nd5, tb=0, R50=48, wv=0.42,  }  
20.e4 { ev=0.54, d=26, pd=Bxd4, mt=00:04:22, tl=01:21:34, s=18725 kN/s, n=4921124085, pv=e4 Bxd4 exf5 gxf5 Bf4 e5 Bh5 Kg8 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Bf3 Kf7 g3 Kg7 Kg2 Kg6 g4 Nb5 Rd1, tb=0, R50=50, wv=0.54,  }  
Bxd4 { ev=-0.34, d=32, pd=exf5, mt=00:01:18, tl=01:24:11, s=18657 kN/s, n=1469574656, pv=Bxd4 exf5 gxf5 Bh5 Kg8 Bf4 e5 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Rd1 Kg7 Bf3 Kh6 g3 Kg6 Kg2 Kg5 h4 Kh6, tb=0, R50=50, wv=0.34,  }  
21.exf5 { ev=0.53, d=26, pd=gxf5, mt=00:02:35, tl=01:19:29, s=20845 kN/s, n=3247118248, pv=exf5 gxf5 Bh5 Kg8 Bf4 e5 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nc7 Rxd4 Nb5 Rd1 Nd5 Bf3 Nbc7 g4 fxg4 hxg4 Nxb4 Rd7 Ncd5 Nxb7, tb=0, R50=50, wv=0.53,  }  
gxf5 { ev=-0.42, d=34, pd=Bh5, mt=00:02:29, tl=01:22:13, s=20927 kN/s, n=3118475249, pv=gxf5 Bh5 Kg8 Bf4 e5 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Rd1 f4 Rd4 Nb5 Rd3 Nbc7 Rd1 h6 Rd3 Ne8 Rd4 Nef6, tb=0, R50=50, wv=0.42,  }  
22.Bf4 { ev=0.49, d=28, pd=e5, mt=00:07:07, tl=01:12:52, s=22306 kN/s, n=9546958061, pv=Bf4 e5 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Ke7 Bf3 Nef6 Rh4 Kf7 g3 Kg6 Rd4 h5 Kg2 Kg5 Bd1 Kg6 Bb3 Kg5 Bc2, tb=0, R50=49, wv=0.49,  }  
e5 { ev=-0.54, d=36, pd=Bh5, mt=00:01:38, tl=01:21:05, s=19932 kN/s, n=1959582427, pv=e5 Bh5 Kg8 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Rd1 f4 Rd4 Nb5 Rd3 Nbc7 Rd1 h6 Rd4 Nb5 Re4 Nf6 Re5 Nc7, tb=0, R50=50, wv=0.54,  }  
23.Bh5+ { ev=0.51, d=27, pd=Kg8, mt=00:02:30, tl=01:10:52, s=22206 kN/s, n=3362022616, pv=Bh5 Kg8 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd6 Rxd4 Nb5 Rd1 Nd5 Be2 Kg7 Bc4 Nbc7 g3 Kg6 Kg2 h5 Rd4 Nb5 Rh4 Nf6 Bb3, tb=0, R50=49, wv=0.51,  }  
Kg8 { ev=-0.56, d=36, pd=Rxd4, mt=00:01:26, tl=01:20:10, s=21819 kN/s, n=1880010100, pv=Kg8 Rxd4 exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Rd1 f4 Rd4 Nb5 Rd3 Nbc7 Rd1 Kg7 Re1 Kg8 Bf3 h6 Re4 Kg7 Kf1 Kg6, tb=0, R50=49, wv=0.56,  }  
24.Rxd4 { ev=0.52, d=27, pd=exd4, mt=00:01:29, tl=01:09:53, s=22468 kN/s, n=2034374447, pv=Rxd4 exd4 Bxb8 Rxb8 Rd1 Kg7 Bf3 Nd5 Rxd4 Nef6 Kf1 Kg6 g3 h5 Be2 Ne8 Kg2 Nd6 Bf3 Nb5 Rh4 Nf6 Be2 Nc7 Rd4, tb=0, R50=50, wv=0.52,  }  
exd4 { ev=-0.48, d=36, pd=Bxb8, mt=00:01:48, tl=01:18:52, s=22647 kN/s, n=2451254509, pv=exd4 Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Rd1 b5 g3 Rd8 f4 Kf8 Kf2 Rd6 Bf3 Rh6 Nd7 Ke7 Ne5 Rd6 Nd3 Nb6 Re1 Kf6, tb=0, R50=50, wv=0.48,  }  
25.Bxb8 { ev=0.53, d=26, pd=Rxb8, mt=00:02:38, tl=01:07:46, s=22512 kN/s, n=3582963397, pv=Bxb8 Rxb8 Rd1 Nd5 Rxd4 Nec7 Kh2 Kg7 Bf3 Kf7 g3 Kg7 Kg2 Kf7 Rc4 Kg6 g4 Nf6 Be2 fxg4 hxg4 h5 gxh5 Nxh5 Nd7, tb=0, R50=50, wv=0.53,  }  
Rxb8 { ev=-0.52, d=37, pd=Rd1, mt=00:01:34, tl=01:17:49, s=21937 kN/s, n=2060527533, pv=Rxb8 Rd1 Nd5 Rxd4 Nec7 Rd1 b5 g3 Rd8 f4 Kf8 Kf2 Rd6 Bf3 Rh6 h4 Rd6 Nd3 Ke7 Ra1 Nb6 Re1 Kf6 Rc1 Nc4, tb=0, R50=50, wv=0.52,  }  
26.Rd1 { ev=0.52, d=25, pd=Nd5, mt=00:00:45, tl=01:07:31, s=22461 kN/s, n=1030190815, pv=Rd1 Nd5 Rxd4 Kg7 Bf3 Nec7 Kf1 h6 Rc4 b5 Rd4 Re8 Rd1 Kg6 g3 Kf6 Rd4 Re7 Rh4 Kg7 Kg2 Re1 Nd3 Re7 Rh5, tb=0, R50=49, wv=0.52,  }  
Nd5 { ev=-0.52, d=38, pd=Rxd4, mt=00:01:52, tl=01:16:26, s=21448 kN/s, n=2616184509, pv=Nd5 Rxd4 Nec7 Rd1 b5 g3 Rd8 f4 Kf8 Kf2 Rd6 Bf3 Rh6 h4 Rd6 Nd3 Ke7 Ra1 Nb6 Re1 Kf6 Rc1 Nc4 b3 Nd2, tb=0, R50=49, wv=0.52,  }  
27.Rxd4 { ev=0.52, d=26, pd=Nec7, mt=00:00:52, tl=01:07:09, s=22317 kN/s, n=1181957570, pv=Rxd4 Nec7 Bf3 Kf7 Kh2 h6 g3 Nf6 Rh4 Kg6 Kg2 h5 Rd4 Ncd5 Rc4 Nc7 Rh4 Nb5 Kf1 Nc7 Rc4 Kg5 Rd4 Nfd5 Kg2, tb=0, R50=50, wv=0.52,  }  
Nec7 { ev=-0.68, d=37, pd=Bf3, mt=00:11:51, tl=01:05:06, s=24031 kN/s, n=17087850847, pv=Nec7 Bf3 Kg7 Kh2 Kf7 g4 Ke7 gxf5 b6 Ne4 Rf8 Rc4 Rxf5 Bg4 Re5 Ng3 Ne6 Rxc6, tb=0, R50=49, wv=0.68,  }  
28.Bf3 { ev=0.53, d=26, pd=h6, mt=00:01:14, tl=01:06:26, s=22252 kN/s, n=1668022467, pv=Bf3 h6 g3 Kg7 Kg2 Kf7 Rh4 Kg7 Rc4 Kg6 Bd1 Nb5 Bc2 h5 Rh4 Nbc7 Bd3 Ne8 Be2 Nef6 Rd4 Re8 Bd3 Re7 g4, tb=0, R50=49, wv=0.53,  }  
Kf7 { ev=-0.80, d=34, pd=g3, mt=00:02:10, tl=01:03:27, s=22149 kN/s, n=2879316589, pv=Kf7 g3 Kg6 Kg2 Kf7 Kh2 Nf6 Bd1 Ke7 Bc2 Nb5 Rd1 b6 Re1 Kf7 Ne6 Nd5 Bxf5 h6 Nf4 Nd4 Bg6 Kg7 Re4 Nf3, tb=0, R50=48, wv=0.80,  }  
29.Kh2 { ev=0.53, d=25, pd=h6, mt=00:02:06, tl=01:04:50, s=22272 kN/s, n=2822019119, pv=Kh2 h6 g3 Nf6 Be2 Nfd5 Rh4 Kg6 Kg2 Nb5 Bf3 Nf6 Rf4 Nc7 Rd4 Nfd5 g4 Nb5 Rd1 Nf6 Be2 h5 gxh5 Kh6 Bf3, tb=0, R50=48, wv=0.53,  }  
Nf6 { ev=-0.62, d=35, pd=Rd6, mt=00:02:11, tl=01:01:46, s=21662 kN/s, n=2848166692, pv=Nf6 Rd6 Nce8 Rd3 Nc7 g4 fxg4 hxg4 Ne6 Kg3 Nxc5 bxc5 h6 Kh4 a5 Re3 Rd8 Rb3 Rd7 Bg2 Nd5 Be4 Kf6 f3 Nb4, tb=0, R50=47, wv=0.62,  }  
30.Bd1 { ev=0.56, d=28, pd=Ncd5, mt=00:03:15, tl=01:02:05, s=18906 kN/s, n=3702145832, pv=Bd1 Ncd5 g4 fxg4 hxg4 b6 Nxa6 Rg8 b5 c5 Rc4 Rg5 b4 cxb4 Nxb4 Nxb4 Rxb4 h5 gxh5 Nxh5 Kh3 Re5 Bf3 Rf5 Bg4, tb=0, R50=47, wv=0.56,  }  
Ncd5 { ev=-0.64, d=34, pd=g3, mt=00:01:29, tl=01:00:47, s=20807 kN/s, n=1860008814, pv=Ncd5 g3 Ke7 Bc2 Kd6 Rd1 Re8 Bxf5 Re2 Kg1 Kc7 Nd3 Ne7 Kf1 Rc2 Be6 Nfd5 Bg4 Kb6 b3 Ka7 Nc5 Rc3 Rd3 Rc1, tb=0, R50=46, wv=0.64,  }  
31.g4 { ev=0.57, d=28, pd=fxg4, mt=00:04:13, tl=00:58:23, s=22544 kN/s, n=5715181655, pv=g4 fxg4 hxg4 b6 Nxa6 Rg8 b5 c5 Rc4 Rg5 b4 Nxb4 Nxb4 cxb4 Rxb4 h5 gxh5 Nxh5 Kh3 Rc5 Bf3 Rf5 Bg4 Rc5 Bd7, tb=0, R50=50, wv=0.57,  }  
h6 { ev=-0.82, d=35, pd=gxf5, mt=00:01:27, tl=00:59:50, s=20539 kN/s, n=1802062055, pv=h6 gxf5 Ke7 Rh4 h5 Kg3 Rg8 Kf3 b5 Nxa6 Kd6 Nc5 Ke5 Ne6 Rg1 Ke2 Rh1 Bc2 Kd6 Ng5 Rc1 Bd3 Rg1 Ne6 Ra1, tb=0, R50=50, wv=0.82,  }  
32.gxf5 { ev=0.60, d=25, pd=Ke7, mt=00:05:20, tl=00:53:33, s=3330 kN/s, n=1069159122, pv=gxf5 Ke7 Rh4 h5 Kg3 Rg8 Kf3 Rb8 Bb3 Kd6 Ne4 Ke5 Bxd5 cxd5 Nxf6 Kxf6 Rxh5 Rd8 b3 d4 Ke2 d3 Kd2 b5 Rh6, tb=0, R50=50, wv=0.60,  }  
Ke7 { ev=-0.78, d=37, pd=Rh4, mt=00:01:09, tl=00:59:11, s=20737 kN/s, n=1442876766, pv=Ke7 Rh4 h5 Kg3 Rg8 Kf3 b5 Nxa6 Kd6 Nc5 Rg1 Ke2 Ke5 Ne6 Rh1 Bc2 Kd6 Nd8 Kc7 Nf7 Rc1 Bd3 Rg1 Ne5 Kd6, tb=0, R50=49, wv=0.78,  }  
33.Kg3 { ev=0.67, d=29, pd=Kd6, mt=00:03:52, tl=00:50:12, s=22573 kN/s, n=5243033029, pv=Kg3 Kd6 Rh4 Rg8 Kf3 b6 Ne4 Ke5 Bc2 Kxf5 Nxf6 Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 Bd3 b5 Be4 Rf7 Bf3, tb=0, R50=49, wv=0.67,  }  
Kd6 { ev=-1.03, d=35, pd=Rh4, mt=00:03:36, tl=00:56:06, s=22382 kN/s, n=4832102826, pv=Kd6 Rh4 Rg8 Kf3 b6 Nxa6 h5 Bb3 Rg5 Bc2 Rg8 b5 c5 b4 Rg1 bxc5 bxc5 Rc4 Nd7 Ke2 Ra1 Be4 Ra2 Kf1 Nf4, tb=0, R50=48, wv=1.03,  }  
34.Rh4 { ev=0.69, d=27, pd=Rg8, mt=00:00:47, tl=00:49:54, s=22940 kN/s, n=1109422691, pv=Rh4 Rg8 Kf3 b6 Ne4 Ke5 Bc2 Kxf5 Nxf6 Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 f3 b5 Rh6 Kd5 f4 c5 bxc5, tb=0, R50=48, wv=0.69,  }  
Rg8+ { ev=-0.96, d=36, pd=Kf3, mt=00:02:51, tl=00:53:45, s=23639 kN/s, n=4047301070, pv=Rg8 Kf3 b6 Nxa6 h5 Bb3 Rg5 Bc2 Rg8 b5 c5 b4 Rg1 bxc5 bxc5 Rc4 Nd7 Ke2 Ra1 Be4 Ra2 Rc2 Rxc2 Bxc2 c4, tb=0, R50=47, wv=0.96,  }  
35.Kf3 { ev=0.74, d=27, pd=b6, mt=00:00:43, tl=00:49:42, s=23529 kN/s, n=1016046397, pv=Kf3 b6 Ne4 Ke5 Bc2 Kxf5 Nxf6 Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 f3 Rg8 Kf2 Rc8 Rd4 Ke5 Rd1 Nd5 Re1, tb=0, R50=47, wv=0.74,  }  
b6 { ev=-1.03, d=37, pd=Ne4, mt=00:01:47, tl=00:52:29, s=21995 kN/s, n=2352191797, pv=b6 Ne4 Ke5 Rxh6 Nxe4 Re6 Kxf5 Rxe4 Rh8 h4 Nf6 Rf4 Ke6 Ba4 b5 Bb3 Ke5 Bc2 Ke6 Bg6 Rh6 Bf5 Ke5 Be4 Nd5, tb=0, R50=50, wv=1.03,  }  
36.Ne4+ { ev=0.72, d=28, pd=Ke5, mt=00:02:02, tl=00:48:10, s=13563 kN/s, n=1672680987, pv=Ne4 Ke5 Bc2 Kxf5 Nxf6 Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 f3 Rg8 Kf2 Rc8 Rf4 Ke5 Rf5 Ke6 Rg5 Rh8 Kg3, tb=0, R50=49, wv=0.72,  }  
Ke5 { ev=-0.98, d=39, pd=Rxh6, mt=00:01:13, tl=00:51:45, s=21817 kN/s, n=1610888716, pv=Ke5 Rxh6 Nxe4 Re6 Kxf5 Rxe4 Rh8 h4 Nf6 Rf4 Ke6 Ba4 b5 Bb3 Ke5 Bc2 Ke6 Bg6 Rh6 Bf5 Ke5 Be4 Nd5 Bxd5 cxd5, tb=0, R50=49, wv=0.98,  }  
37.Bc2 { ev=0.73, d=28, pd=Kxf5, mt=00:02:24, tl=00:46:16, s=23160 kN/s, n=3337785673, pv=Bc2 Kxf5 Nxf6 Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 f3 Rg8 Kf2 Rc8 Rf4 Ke5 Rf5 Ke6 Rg5 Rh8 f4 Kd6 Kg3, tb=0, R50=48, wv=0.73,  }  
Kxf5 { ev=-0.98, d=38, pd=Rxh6, mt=00:01:04, tl=00:51:11, s=20282 kN/s, n=1307439773, pv=Kxf5 Rxh6 Ke5 Nxf6 Nxf6 Rh4 Kd6 Rc4 Rb8 h4 Rh8 Bg6 b5 Rd4 Ke6 Rf4 Rh6 Bf5 Ke5 Be4 Nd5 Bxd5 cxd5 Ke3 Rh7, tb=0, R50=50, wv=0.98,  }  
38.Nxf6+ { ev=0.73, d=28, pd=Kxf6, mt=00:01:31, tl=00:45:16, s=19957 kN/s, n=1818843406, pv=Nxf6 Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 Rd4 Ke5 Rc4 Kd6 Kg2 Rg8 Kf3 b5 Rf4 Ke5 h4 Rg1 Rf5 Ke6 Rg5, tb=0, R50=50, wv=0.73,  }  
Kxf6 { ev=-0.98, d=33, pd=Rxh6, mt=00:00:20, tl=00:51:21, s=18200 kN/s, n=377122760, pv=Kxf6 Rxh6 Ke5 Rh5 Kd6 Rh4 Nf6 Rc4 Rb8 h4 Rh8 Bg6 b5 Rd4 Ke6 Rf4 Rh6 Bf5 Ke5 Be4 Nd5 Bxd5 cxd5 Ke3 Rh7, tb=0, R50=50, wv=0.98,  }  
39.Rxh6+ { ev=0.71, d=26, pd=Ke5, mt=00:00:28, tl=00:45:18, s=23313 kN/s, n=667727965, pv=Rxh6 Ke5 Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 f3 Ke5 Rc4 Rg8 Kf2 Rd8 f4 Kd5 Bd3 Rh8 Kg3 Rg8 Kh4 b5 Rc3 Kd6 Bf5, tb=0, R50=50, wv=0.71,  }  
Ke5 { ev=-0.98, d=37, pd=Rh5, mt=00:01:06, tl=00:50:45, s=22357 kN/s, n=1480200115, pv=Ke5 Rh5 Kd6 Rh4 Nf6 Rc4 Rb8 h4 Rh8 Bg6 b5 Rd4 Ke6 Rf4 Rh6 Bf5 Ke5 Be4 Nd5 Bxd5 cxd5 Ke3 Rh7 Rg4 Rh8, tb=0, R50=49, wv=0.98,  }  
40.Rh5+ { ev=0.72, d=27, pd=Kd6, mt=00:00:59, tl=00:44:49, s=23100 kN/s, n=1379492877, pv=Rh5 Kd6 Rh4 Rf8 Kg3 Nf6 f3 Rg8 Kf2 Rb8 Rd4 Ke5 Rc4 Kd6 Be4 c5 Bf5 Kd5 Bd3 cxb4 Rxb4 a5 Rc4 Ke5 f4, tb=0, R50=49, wv=0.72,  }  
Kd6 { ev=-0.98, d=38, pd=Rh4, mt=00:02:17, tl=00:48:58, s=23289 kN/s, n=3202599280, pv=Kd6 Rh4 Nf6 Rc4 Rb8 h4 Rh8 Bg6 b5 Rd4 Ke6 Rf4 Rh6 Bf5 Ke5 Be4 Nd5 Bxd5 cxd5 Ke3 Rh7 Rg4 Rh8 Rd4 Rh5, tb=0, R50=48, wv=0.98,  }  
41.Rh4 { ev=0.73, d=27, pd=Rf8, mt=00:02:33, tl=00:42:47, s=19449 kN/s, n=2985941436, pv=Rh4 Rf8 Kg3 Nf6 f3 Rg8 Kf2 Ke5 Rc4 Rd8 Kg3 Rg8 Kh2 Kd6 h4 Rh8 Kh3 b5 Rf4 Ke6 Bg6 Rh6 Be4 Rh8 Rf5, tb=0, R50=48, wv=0.73,  }  
Nf6 { ev=-0.98, d=39, pd=Rc4, mt=00:02:39, tl=00:46:49, s=23268 kN/s, n=3712651549, pv=Nf6 Rc4 Rb8 h4 Rh8 Rf4 Ke6 Ba4 b5 Bb3 Ke5 Bc2 Rh6 Rf5 Kd4 Kf4 Nd5 Kg5 Rh8 Rf7 Nxb4 Bg6 Nd5 h5 a5, tb=0, R50=47, wv=0.98,  }  
42.Rd4+ { ev=0.85, d=26, pd=Ke5, mt=00:01:31, tl=00:41:46, s=23087 kN/s, n=2110176502, pv=Rd4 Ke5 Rc4 Rf8 h4 Nd5 Kg3 Rg8 Kh2 Rf8 Re4 Kd6 Kg3 Nf6 Rd4 Ke5 Rc4 Kd6 Kg2 Nh5 f3 Nf6 Kh3 Kd5 Bb3, tb=0, R50=47, wv=0.85,  }  
Ke5 { ev=-0.98, d=40, pd=Rc4, mt=00:01:18, tl=00:46:02, s=22082 kN/s, n=1721098426, pv=Ke5 Rc4 Kd6 h4 Rh8 Rf4 Ke6 Ba4 b5 Bb3 Ke5 Bc2 Ke6 Rd4 Ke5 Rf4, tb=0, R50=46, wv=0.98,  }  
43.Rc4 { ev=0.86, d=26, pd=Nd5, mt=00:01:07, tl=00:41:10, s=24141 kN/s, n=1637132054, pv=Rc4 Nd5 Re4 Kd6 Rg4 Rf8 Kg3 Nf6 Rg6 Ke7 Rg7 Ke6 Rg5 Kd6 Bd3 Rh8 f4 Nh5 Kf3 Nf6 Bf5 Nd5 Rg6 Kc7 Rg7, tb=0, R50=46, wv=0.86,  }  
Kd6 { ev=-1.15, d=40, pd=Rf4, mt=00:09:41, tl=00:36:52, s=24077 kN/s, n=14124694124, pv=Kd6 Rf4 Ke7 h4 Rg1, tb=0, R50=45, wv=1.15,  }  
44.h4 { ev=0.85, d=28, pd=Rh8, mt=00:01:54, tl=00:39:46, s=18868 kN/s, n=2174131272, pv=h4 Rh8 Bg6 Ke7 Bf5 Kd6 Rf4 Ke5 Bg6 Ke6 Bd3 Rd8 Bc2 Ke5 Rc4 Rf8 Kg2 Kd6 Bd3 Nh5 Be2 b5 Rd4 Ke5 Rd2, tb=0, R50=50, wv=0.85,  }  
Rh8 { ev=-0.98, d=41, pd=Rf4, mt=00:00:53, tl=00:36:29, s=21664 kN/s, n=1156190099, pv=Rh8 Rf4 Ke6 Ba4 b5 Bb3 Ke5 Bc2 Ke6 Bg6 Rh6 Bf5 Ke5 Bc2 Kd6 Bb1 Ke6 Ba2 Ke5 Ke3 Rh8 Rd4 Nd5 Bxd5 cxd5, tb=0, R50=49, wv=0.98,  }  
45.Bf5 { ev=0.85, d=25, pd=Kd5, mt=00:01:47, tl=00:38:29, s=17023 kN/s, n=1838495310, pv=Bf5 Kd5 Bd3 Ke5 Rf4 Ke6 Bg6 Rh6 Bf5 Ke5 Bd3 Rh8 Bc2 Ke6 Bf5 Ke5 Bg6 Ke6 Rc4 Kd5 Bf7 Kd6 Rd4 Ke5 Rf4, tb=0, R50=49, wv=0.85,  }  
Kd5 { ev=-0.98, d=40, pd=Bd3, mt=00:00:58, tl=00:36:01, s=21892 kN/s, n=1271869160, pv=Kd5 Bd3 b5 Rf4 Ke6 Bf5 Ke5 Bc2 Ke6 Bg6 Rh6 Bf5 Ke5 Bc2 Kd6 Bb1 Ke6 Ba2 Ke5 Ke3 Rh8 Rd4 Nd5 Bxd5 cxd5, tb=0, R50=48, wv=0.98,  }  
46.Bd3 { ev=0.86, d=27, pd=Nd7, mt=00:00:48, tl=00:38:10, s=23871 kN/s, n=1172283690, pv=Bd3 Nd7 Ke3 Nf6 Rd4 Ke6 Bg6 Rh6 Bc2 Rh8 Kf3 Ke5 Rf4 Rd8 Bg6 Rd2 Kg3 Nd5 Rf5 Ke6 h5 Rxb2 h6 Nf6 Rf4, tb=0, R50=48, wv=0.86,  }  
b5 { ev=-1.15, d=42, pd=Rf4, mt=00:08:01, tl=00:28:30, s=24743 kN/s, n=11914412982, pv=b5 Rf4 Ke6 Bf5 Kd6 Bb1 Ke7 Rd4 Ke6 Bc2 Ke5 Rf4 Nd5 Re4 Kf6 Kg2 Rh6 Rd4 Ke6 Kg3 Ke5 Rg4 Kf6 Bb1 Ke5, tb=0, R50=50, wv=1.15,  }  
47.Rf4 { ev=0.76, d=26, pd=Ke6, mt=00:00:44, tl=00:37:56, s=25491 kN/s, n=1152450075, pv=Rf4 Ke6 Bg6 Ke7 Bf5 Kd6 Bb1 Ke6 Ba2 Ke5 Bb3 Rd8 Bc2 Ke6 Bg6 Rh8 Bb1 Rd8 Ke2 Rh8 Bg6 Nd5 Rd4 Ke5 Re4, tb=0, R50=49, wv=0.76,  }  
Ke6 { ev=-0.98, d=41, pd=Bc2, mt=00:01:11, tl=00:27:49, s=21701 kN/s, n=1568310693, pv=Ke6 Bc2 Rh6 Bb1 Rh8 Ba2 Ke5 Bb1 Ke6, tb=0, R50=49, wv=0.98,  }  
48.Bg6 { ev=0.77, d=29, pd=Ke7, mt=00:02:44, tl=00:35:42, s=15402 kN/s, n=2540066225, pv=Bg6 Ke7 Ke2 Rh6 Bf5 Kd6 Bc2 Ke5 Kf3 Rh8 Bb1 Ke6 Bf5 Ke5 Bh3 Rh6 Bc8 Kd6 Bf5 Ke5 Bb1 Ke6 Be4 Nd5 Bxd5, tb=0, R50=48, wv=0.77,  }  
Rh6 { ev=-0.98, d=40, pd=Bf5, mt=00:02:37, tl=00:25:42, s=20735 kN/s, n=3269718263, pv=Rh6 Bf5 Ke5 Bb1 Ke6 Ba2 Ke5 Bf7 Kd6 Ba2 Ke5, tb=0, R50=48, wv=0.98,  }  
49.Bf5+ { ev=0.93, d=26, pd=Kd5, mt=00:01:22, tl=00:34:50, s=24169 kN/s, n=2008523377, pv=Bf5 Kd5 Bb1 Ke6 Kg2 Rh5 Kg3 c5 bxc5 Rxc5 Rf3 Nh5 Kh3 a5 Ba2 Ke5 Bf7 Nf6 Bg6 Ke6 Re3 Kd6 Rd3 Ke7 Rg3, tb=0, R50=47, wv=0.93,  }  
Kd6 { ev=-1.43, d=40, pd=Bb1, mt=00:03:05, tl=00:23:07, s=22303 kN/s, n=4130618123, pv=Kd6 Bb1 Ke7 Ba2 Rh8 Rd4 Rh6 Kg2 Rg6 Kf1 Rh6 f3 Nd5 Re4 Kf6 Kf2 Rh5 Kg3 Rh7 Bb1 Rh8 Bc2 Kf7 Rg4 Kf6, tb=0, R50=47, wv=1.43,  }  
50.Be4 { ev=0.94, d=26, pd=Kd7, mt=00:02:06, tl=00:33:14, s=20919 kN/s, n=2657794772, pv=Be4 Kd7 Kg2 Nd5 Bf5 Ke7 Rg4 Nf6 Rd4 Rh8 Kg3 Rg8 Kh2 Rh8 Kg2 Rg8 Kf3 Rf8 Bh3 Rf7 Kg3 Rg7 Kf4 Nd5 Kf3, tb=0, R50=46, wv=0.94,  }  
Kd7 { ev=-1.43, d=40, pd=Bb1, mt=00:01:13, tl=00:22:25, s=20710 kN/s, n=1521405869, pv=Kd7 Bb1 Ke7 Ba2 Rh8 Rd4 Rh6 Kg2 Rg6 Kf1 Rh6 f3 Nd5 Re4 Kf6 Kf2 Rh5 Kg3 Rh7 Bb1 Rh8 Bc2 Kf7 Rg4 Kf6, tb=0, R50=46, wv=1.43,  }  
51.Kg2 { ev=0.97, d=28, pd=Nd5, mt=00:01:12, tl=00:32:32, s=22740 kN/s, n=1646409049, pv=Kg2 Nd5 Bf5 Kd6 Re4 Ne7 Bh3 Nd5 Kg3 Rg6 Rg4 Rh6 Bg2 Ke6 Bf3 Ke5 Bd1 Rd6 Bc2 Nf6 Rg5 Ke6 Bf5 Ke7 Rg7, tb=0, R50=45, wv=0.97,  }  
Kd6 { ev=-1.43, d=35, pd=Bb1, mt=00:00:45, tl=00:22:10, s=20082 kN/s, n=908861798, pv=Kd6 Bb1 Ke7 Kh3 Nd5 Rd4 Kf6 Rg4 Kf7 Kg3 Kf6 f4 Nxb4 f5 Nd5 Rg6 Rxg6 fxg6 Ne7 h5 Kg5 g7 Kh6 Kg4 Kxg7, tb=0, R50=45, wv=1.43,  }  
52.Bb1 { ev=1.07, d=26, pd=Ke7, mt=00:01:00, tl=00:32:02, s=23657 kN/s, n=1443022225, pv=Bb1 Ke7 Kh3 Rh8 Bg6 Rh6 Be4 Nd5 Rf3 Rh8 Bg6 Nxb4 h5 c5 Kg4 c4 Kg5 Nd5 h6 b4 Rf5 Nc7 Rf7 Kd6 Rf6, tb=0, R50=44, wv=1.07,  }  
Ke7 { ev=-1.51, d=35, pd=Kh3, mt=00:00:47, tl=00:21:53, s=22193 kN/s, n=1045592842, pv=Ke7 Kh3 Nd5 Rg4 Kf6 Kg3 Ke7 Bf5 Kf6 Bc2 Rh8 Bd3 Ke7 Bb1 Nf6 Rg7 Ke6 Rg6 Ke7 f4 Re8 Rg5 Kd6 h5 Re1, tb=0, R50=44, wv=1.51,  }  
53.Kh3 { ev=1.13, d=26, pd=Rh8, mt=00:02:12, tl=00:30:20, s=23961 kN/s, n=3182939945, pv=Kh3 Rh8 Bg6 Rh6 Be4 Nd5 Rf3 Rh8 Bg6 Nxb4 Kg4 c5 Kg5 Nd5 h5 c4 h6 b4 Rf7 Kd6 Ra7 Nc7 h7 c3 bxc3, tb=0, R50=43, wv=1.13,  }  
Nd5 { ev=-1.73, d=35, pd=Rg4, mt=00:02:24, tl=00:19:59, s=23528 kN/s, n=3401044408, pv=Nd5 Rg4 Kf7 Kg3 Ke7 Bc2 Kf6 Bd3 Ke7 Bc2, tb=0, R50=43, wv=1.73,  }  
54.Rf3 { ev=1.13, d=24, pd=Nxb4, mt=00:01:15, tl=00:29:36, s=24526 kN/s, n=1852644326, pv=Rf3 Nxb4 Kg4 Rf6 Bf5 Nd5 Kg5 Kf8 h5 Kg7 Be4 Rxf3 Bxf3 a5 h6 Kg8 Bg4 a4 Be6 Kh7 Bf5 Kh8 Bc2 Nb6 f4, tb=0, R50=42, wv=1.13,  }  
Nxb4 { ev=-2.10, d=32, pd=Kg4, mt=00:00:41, tl=00:19:48, s=17527 kN/s, n=730893451, pv=Nxb4 Kg4 Rf6 Bf5 Nd5 Kg5 Kf8 Ra3 Kf7 h5 Ne7 Bg4 Rd6 Bf3 Rf6 Be4 Re6 f3 Re5 Kh4 Nf5 Kg4 Nh6 Kg3 Ke7, tb=0, R50=50, wv=2.10,  }  
55.Kg4 { ev=1.44, d=24, pd=Rf6, mt=00:00:22, tl=00:29:44, s=25114 kN/s, n=572786733, pv=Kg4 Rf6 Bf5 a5 h5 Nd5 Kg5 a4 h6 Kf8 Rd3 Rd6 f4 Kg8 Rd4 Rd8 Be6 Kh7 f5 Re8 Bxd5 cxd5 Rxd5 Rg8 Kh5, tb=0, R50=49, wv=1.44,  }  
Rf6 { ev=-2.34, d=35, pd=Bf5, mt=00:00:55, tl=00:19:23, s=19594 kN/s, n=1090977729, pv=Rf6 Bf5 Nd5 Kg5 Kf8 Ra3 Kg7 Rxa6 Ne7 Be4 Re6 f3 Re5 Kg4 Rc5 h5 Ng8 Ra7 Kf6 b4 Nh6 Kf4 Rxh5 Ra6 Nf7, tb=0, R50=49, wv=2.34,  }  
56.Bf5 { ev=1.26, d=24, pd=Kf8, mt=00:00:28, tl=00:29:46, s=25965 kN/s, n=750983835, pv=Bf5 Kf8 Kg5 Nd5 h5 Kg7 Rd3 a5 f4 Rf8 Rd4 a4 Bg6 Kh8 h6 b4 f5 Ra8 f6 Nxf6 Kxf6 c5 Rd1 a3 bxa3, tb=0, R50=48, wv=1.26,  }  
Nd5 { ev=-2.62, d=37, pd=Kg5, mt=00:01:00, tl=00:18:53, s=19435 kN/s, n=1167922719, pv=Nd5 Kg5 Kf8 Ra3 Kg7 Rxa6 Ne7 Be4 Re6 f3 Re5 Kg4 Rc5 h5 Ng8 h6 Nxh6 Kf4 b4 Bxc6 Ng8 Rb6 Ne7 Be4 Nd5, tb=0, R50=48, wv=2.62,  }  
57.h5 { ev=1.14, d=25, pd=Kf8, mt=00:00:55, tl=00:29:22, s=25280 kN/s, n=1402815659, pv=h5 Kf8 Kg5 Kg7 Rd3 a5 Rd4 a4 f4 b4 Be4 Rd6 Bg6 Re6 h6 Kh8 Bf7 Re2 Bxd5 cxd5 Rxb4 Rg2 Kf6 Kh7 f5, tb=0, R50=50, wv=1.14,  }  
Kf7 { ev=-2.12, d=36, pd=Kg5, mt=00:00:39, tl=00:18:44, s=16797 kN/s, n=667163528, pv=Kf7 Kg5 Kg7 Ra3 Ne7 Bg4 Ng8 f4 Nh6 Bc8 Nf7 Kg4 Rd6 Rxa6 Kf6 Ra7 Nh6 Kf3 Nf5 Rc7 Rd3 Ke2 Rd6 Bd7 b4, tb=0, R50=49, wv=2.12,  }  
58.Kg5 { ev=1.21, d=26, pd=Kg7, mt=00:00:34, tl=00:29:18, s=26224 kN/s, n=918450737, pv=Kg5 Kg7 Rd3 a5 f4 a4 Rd4 Rf8 Bg6 Kh8 h6 b4 f5 Ra8 f6 Nxf6 Kxf6 a3 Rd1 a2 Ra1 b3 Be4 Re8 Bf5, tb=0, R50=49, wv=1.21,  }  
Kg7 { ev=-2.36, d=39, pd=Rd3, mt=00:02:18, tl=00:16:56, s=20001 kN/s, n=2778051940, pv=Kg7 Rd3 a5 f4 Rf8 Rd4 a4 h6 Kh8 Be6 Kh7 Re4 Nf6 Re5 b4 f5 b3 Ra5 Ne4 Kf4 Nf6 Rxa4 Kxh6 Ra6 Rd8, tb=0, R50=48, wv=2.36,  }  
59.Rd3 { ev=1.46, d=27, pd=a5, mt=00:01:44, tl=00:28:04, s=26189 kN/s, n=2747152311, pv=Rd3 a5, tb=0, R50=48, wv=1.46,  }  
a5 { ev=-2.44, d=39, pd=f4, mt=00:00:46, tl=00:16:40, s=20111 kN/s, n=936862769, pv=a5 f4 Rf8 Rd4 a4 Bd7 Kf7 Bxc6 Rg8 Kh4 Ne3 Bd7 Rg1 Rb4 Rh1 Kg3 Rxh5 Bxb5 Kf6 Bxa4 Nf5 Kg2 Nd6 Bd1 Rc5, tb=0, R50=50, wv=2.44,  }  
60.f4 { ev=1.50, d=25, pd=a4, mt=00:00:38, tl=00:27:56, s=25525 kN/s, n=1004264929, pv=f4 a4 Rd4 Rf8 Bg6 Kg8 Re4 b4 f5 Kh8 Rc4 Ra8 Rxc6 a3 bxa3 bxa3 Bf7 Ne7 Rh6 Kg7 Ba2 Nxf5 Rg6 Kh7 Kxf5, tb=0, R50=50, wv=1.50,  }  
a4 { ev=-2.56, d=39, pd=Rd4, mt=00:01:02, tl=00:16:08, s=19746 kN/s, n=1236202016, pv=a4 Rd4 Rf8 Bd7 Kh8 Bxc6 Rg8 Kh4 Ne3 Kh3 Rb8 Re4 Nc4 Re2 Nd6 Kg4 Rg8 Kf3 Rc8 Bd7 Rc7 Be6 Kg7 Re5 b4, tb=0, R50=50, wv=2.56,  }  
61.Rd4 { ev=1.60, d=26, pd=b4, mt=00:00:45, tl=00:27:40, s=25749 kN/s, n=1193840721, pv=Rd4 b4 Be4 Rxf4 h6 Kh8 Rxb4 Rf8 Rxa4 Rg8 Kf5 Re8 Ra3 Kh7 Ra6 Kh8 Ra7 Ne3 Kf4 Nd5 Kf3 Rf8 Kg3 Ne3 Bg6, tb=0, R50=49, wv=1.60,  }  
b4 { ev=-3.05, d=39, pd=Be4, mt=00:03:55, tl=00:12:43, s=21422 kN/s, n=5038948625, pv=b4 Be4 Rxf4 h6 Kh8 Rxb4 Rf8 Rxa4 Rg8 Kh4 Ne3 Bb1 c5 Kh5 Nd5 Bd3 Rd8 Bg6 Rg8 Bb1 Nf6 Kh4 Rb8 Ra2 Kg8, tb=0, R50=50, wv=3.05,  }  
62.Be4 { ev=1.60, d=28, pd=Rxf4, mt=00:00:53, tl=00:27:18, s=25164 kN/s, n=1345369401, pv=Be4 Rxf4 h6 Kh8 Rxb4 Rf8 Rxa4 Rg8 Kf5 Re8 Ra3 Kh7 Ra6 Kh8 Ra7 Ne3 Kf4 Nd5 Kf3 Rf8 Ke2 Nf6 Bxc6 Ng4 Ra8, tb=0, R50=49, wv=1.60,  }  
Rxf4 { ev=-3.23, d=38, pd=h6, mt=00:00:40, tl=00:12:33, s=19599 kN/s, n=788859784, pv=Rxf4 h6 Kh8 Rxb4 Rf8 Rxa4 Rg8 Kf5 Re8 Ra7 Ne3 Kf4 Nd5 Kf3 Rd8 Bg6 c5 Rh7 Kg8 Rb7 Kh8 Ke4 c4 Ke5 Ne3, tb=0, R50=50, wv=3.23,  }  
63.h6+ { ev=1.68, d=26, pd=Kh8, mt=00:00:24, tl=00:27:24, s=27153 kN/s, n=684938612, pv=h6 Kh8 Rxb4 Rf8 Rxa4 Rg8 Kf5 Re8 Ra7 Ne3 Kf4 Nd5 Kf3 Rd8 Bg6 c5 Rh7 Kg8 Rg7 Kh8 Ke4 c4 Rh7 Kg8 Rb7, tb=0, R50=50, wv=1.68,  }  
Kh8 { ev=-3.39, d=39, pd=Rxb4, mt=00:00:52, tl=00:12:12, s=18897 kN/s, n=991435870, pv=Kh8 Rxb4 Rf8 Rxa4 Rg8 Kf5 Re8 Ra3 Kh7 Ra6 Kh8 Bb1 Ne7 Kg5 Rg8 Kh5 Re8 Ra7 Nd5 Bf5 Ne7 Bd3 Nd5 Kg5 Rf8, tb=0, R50=49, wv=3.39,  }  
64.Rxb4 { ev=1.66, d=26, pd=Rf8, mt=00:00:27, tl=00:27:27, s=28204 kN/s, n=784938013, pv=Rxb4 Rf8 Rxa4 Rg8 Kf5 Re8 Ra7 Ne3 Kf4 Nd5 Kf3 Rf8 Kg3 Ne3 Bg6 Nc4 Rh7 Kg8 Rb7 c5 Kh4 Rd8 Kg5 Rd5 Kf4, tb=0, R50=50, wv=1.66,  }  
Rf8 { ev=-3.39, d=41, pd=Rxa4, mt=00:00:34, tl=00:12:07, s=16725 kN/s, n=582664321, pv=Rf8 Rxa4 Rg8 Kf5 Re8 Ra3 Kh7 Ra6 Kh8 Bb1 Ne7 Kg5 Rg8 Kh5 Rb8 Bc2 Rb5 Kg4 Rb4 Kg3 Rb8 Ra7 Nd5 Kh4 Ne3, tb=0, R50=49, wv=3.39,  }  
65.Rxa4 { ev=1.66, d=24, pd=Rg8, mt=00:00:19, tl=00:27:38, s=27502 kN/s, n=550464666, pv=Rxa4 Rg8 Kf5 Re8 Ra7 Ne3 Kf4 Nd5 Kf3 Rf8 Kg3 Ne3 Bg6 Nc4 Rh7 Kg8 Rb7 c5 Kh4 Ne5 Bh7 Kh8 Kh5 c4 Be4, tb=0, R50=50, wv=1.66,  }  
Rg8+ { ev=-4.62, d=42, pd=Kf5, mt=00:04:28, tl=00:08:10, s=18405 kN/s, n=5006201437, pv=Rg8 Kf5 Nc7 Ke5 Rb8 Kd6 Ne8 Kxc6 Rd8 b4 Rd4 Bf5 Rd6 Kc5 Rf6 Be4 Nc7 b5 Ne6 Kd6 Rxh6 Ra6 Ng5 Ke5 Nf7, tb=0, R50=49, wv=4.62,  }  
66.Kf5 { ev=1.66, d=24, pd=Re8, mt=00:00:18, tl=00:27:50, s=27831 kN/s, n=521453887, pv=Kf5 Re8 Ra7 Ne3 Kf4 Nd5 Kf3 Rd8 Rh7 Kg8 Rg7 Kh8 Rf7 Kg8 Bg6 c5 Ke4 c4 Rb7 Kh8 Ke5 Ne3 Rh7 Kg8 Rg7, tb=0, R50=49, wv=1.66,  }  
Nb6 { ev=-4.24, d=39, pd=Ra7, mt=00:00:46, tl=00:07:54, s=13286 kN/s, n=620770403, pv=Nb6 Ra7 Rd8 Kg6 Nd5 Bf5 Nf4 Kf6 Nd5 Kg5 Rg8 Bg6 Rd8 Rh7 Kg8 Bb1 Ra8 Rg7 Kh8 Rd7 Rb8 Be4 Ra8 Rb7 Rg8, tb=0, R50=48, wv=4.24,  }  
67.Ra7 { ev=1.82, d=25, pd=Rf8, mt=00:00:56, tl=00:27:25, s=27386 kN/s, n=1540626068, pv=Ra7 Rf8 Kg6 Nd5 Kg5 Rg8 Bg6 Rb8 Rh7 Kg8 Rg7 Kh8 Ra7 Ne3 Rh7 Kg8 Rg7 Kh8 Be4 Nd5 Rf7 Kg8 Bg6 Ne3 Rf6, tb=0, R50=48, wv=1.82,  }  
Rd8 { ev=-5.49, d=39, pd=Kg6, mt=00:00:27, tl=00:07:57, s=14280 kN/s, n=386981247, pv=Rd8 Kg6 Nd5 Bf5 Nf4 Kg5 Ne2 b4 Nd4 Be4 Re8 Bb1 Rg8 Kf6 Rd8 Be4 Ne2 Rh7 Kg8 Rb7 Rf8 Kg5 Nd4 Rg7 Kh8, tb=0, R50=47, wv=5.49,  }  
68.Kg6 { ev=2.37, d=26, pd=Nd5, mt=00:01:07, tl=00:26:48, s=25914 kN/s, n=1754924098, pv=Kg6 Nd5, tb=0, R50=47, wv=2.37,  }  
Nd5 { ev=-6.26, d=41, pd=Bf5, mt=00:00:57, tl=00:07:30, s=13711 kN/s, n=784625644, pv=Nd5 Bf5 Nf4 Kg5 Nd5 Rh7 Kg8 Rg7 Kh8 Bd7 c5 Bf5 Ne3 Be6 Rd1 Re7 Rg1 Kf4 Nc2 Re8 Kh7 Bf5 Kxh6 Bxc2 Rf1, tb=0, R50=46, wv=6.26,  }  
69.Bf5 { ev=2.38, d=27, pd=Rd6, mt=00:01:26, tl=00:25:51, s=21903 kN/s, n=1908353467, pv=Bf5 Rd6 Kg5 Rd8 Rh7 Kg8 Rd7 Rf8 b4 Kh8 b5 Nf6 Rd6 cxb5 Rxf6 Rxf6 Kxf6 b4 Kg5 b3 Kg6 Kg8 Kf6 b2 Bb1, tb=0, R50=46, wv=2.38,  }  
Nf4+ { ev=-6.62, d=42, pd=Kg5, mt=00:00:35, tl=00:07:26, s=11504 kN/s, n=403827329, pv=Nf4 Kg5 Nd5 Rh7 Kg8 Rg7 Kh8 Bd7 c5 Bf5 Ne3 Be6 Rd1 Re7 Rg1 Kf4 Nc2 Re8 Kh7 Bf5 Kxh6 Bxc2 Rf1 Kg3 Kg7, tb=0, R50=45, wv=6.62,  }  
70.Kg5 { ev=2.32, d=25, mt=00:00:31, tl=00:25:50, s=32345 kN/s, n=1027845318, pv=Kg5, tb=0, R50=45, wv=2.32,  }  
Ne2 { ev=-6.82, d=44, pd=b4, mt=00:00:43, tl=00:07:13, s=12458 kN/s, n=537381739, pv=Ne2 b4 Nd4 Be4 Re8 Bg6 Rf8 Rh7 Kg8 Re7 Nf3 Kg4 Nd4 Bh7 Kh8 Be4 Kg8 Kg5 Rd8 Kf6 Rd6 Ke5 Rd8 Kf6, tb=0, R50=44, wv=6.82,  }  
71.Bb1 { ev=2.80, d=25, pd=Nd4, mt=00:00:53, tl=00:25:27, s=30061 kN/s, n=1625883663, pv=Bb1 Nd4 Kg6 Rd6 Kf7 Rd8 Rc7 Rb8 b4 Rd8 Kf6 Kg8 Rg7 Kh8 Ke7 Rd5 Rf7 Re5 Kd8 Rd5 Rd7 Ne6 Ke7 Nf4 Rc7, tb=0, R50=44, wv=2.80,  }  
Rg8+ { ev=-6.82, d=43, pd=Bg6, mt=00:00:59, tl=00:06:45, s=11718 kN/s, n=694850138, pv=Rg8 Bg6 Rd8 Rh7 Kg8 Re7 Nd4 b4 Kf8 Rf7 Kg8 Rg7 Kh8 Re7 Nf3 Kg4 Nd4 Be4 Rf8 Kg5 Rd8 Kg6 Kg8 Rg7 Kf8, tb=0, R50=43, wv=6.82,  }  
72.Kf6 { ev=3.29, d=23, pd=Rf8, mt=00:00:20, tl=00:25:38, s=31085 kN/s, n=636227816, pv=Kf6 Rf8 Rf7 Rxf7 Kxf7 Nf4 Kf6 Nd5 Ke5 Nb6 Bd3 Na4 b4 Nc3 Kd6 Nd5 Kc5 Nc3 Bf5 Na2 Bg6 Nxb4 Kxb4 c5 Kc4, tb=0, R50=43, wv=3.29,  }  
Rb8 { ev=-6.82, d=42, pd=Rh7, mt=00:00:28, tl=00:06:47, s=10625 kN/s, n=305491585, pv=Rb8 Rh7 Kg8 Rc7 Rf8 Kg5 Rd8 Rg7 Kh8 Rh7 Kg8 Re7 Nd4 b4 Nf3 Kg6 Rd6 Kh5 Rd8 Be4 Nd4 Kg5 Rf8 Kg6 Rd8, tb=0, R50=42, wv=6.82,  }  
73.Rh7+ { ev=3.46, d=25, pd=Kg8, mt=00:00:42, tl=00:25:26, s=29635 kN/s, n=1263035475, pv=Rh7 Kg8, tb=0, R50=42, wv=3.46,  }  
Kg8 { ev=-6.82, d=12, pd=Rc7, mt=00:00:00, tl=00:07:17, s=948 kN/s, n=948, pv=Kg8 Rc7 Rf8 Kg5 Rd8 Rg7 Kh8 Rh7 Kg8 Re7 Nd4 b4 Nf3 Kg6 Rd6 Kh5 Rd8 Be4 Nd4 Kg5 Rf8 Kg6 Rd8 Rg7 Kh8, tb=0, R50=41, wv=6.82,  }  
74.Rc7 { ev=3.92, d=24, pd=Rf8, mt=00:00:20, tl=00:25:37, s=30953 kN/s, n=636424900, pv=Rc7 Rf8 Kg5 Rd8 Rxc6 Nd4 Rf6 Kh8 b4 Rg8 Kh5 Rd8 Bd3 Nb3 Kg6 Rg8 Kf7 Nc1 Rg6 Rd8 Bf5 Nd3 Ke7 Rd5 Bxd3, tb=0, R50=41, wv=3.92,  }  
Rf8+ { ev=-8.54, d=42, pd=Kg5, mt=00:01:30, tl=00:06:17, s=14704 kN/s, n=1328868368, pv=Rf8 Kg5 Rd8 Rxc6 Nd4 Rg6 Kh8 Rb6 Kg8 Bg6 Ne2 Re6 Nd4 Re8 Rxe8 Bxe8 Ne6 Kf6 Nd4 Bg6 Nc6 Be4 Nb4 Ke6 Na6, tb=0, R50=40, wv=8.54,  }  
75.Kg5 { ev=4.08, d=24, pd=Rd8, mt=00:00:15, tl=00:25:52, s=31568 kN/s, n=481137862, pv=Kg5 Rd8 Rxc6 Nd4 Rf6 Ne2 b4 Kh8 Bf5 Ng3 Bg6 Kg8 Re6 Rb8 Re5 Nf1 Re8 Rxe8 Bxe8 Ne3 b5 Nd5 Bd7 Nb6 Bc6, tb=0, R50=40, wv=4.08,  }  
Rd8 { ev=-10.62, d=43, pd=Rxc6, mt=00:01:28, tl=00:05:19, s=14609 kN/s, n=1285922662, pv=Rd8 Rxc6 Nd4 Rg6 Kh8 Rb6 Rc8 Kf6 Kg8 Bg6 Rf8 Kg5 Rd8 b4 Kh8 Kf6 Ra8 Rd6 Nb5 Rd7 Rf8 Kg5 Na3 Be4 Nc4, tb=0, R50=39, wv=10.62,  }  
76.Rxc6 { ev=4.08, d=29, pd=Nd4, mt=00:00:27, tl=00:25:55, s=30546 kN/s, n=864429078, pv=Rxc6 Nd4 Rf6 Rb8 Kg6 Kh8 b4 Rg8 Kh5 Rd8 Bd3 Nb3 Kg6 Rg8 Kf7 Nd2 b5 Rd8 Kg6 Rg8 Kh5 Rd8 Bf5 Nc4 b6, tb=0, R50=50, wv=4.08,  }  
Nd4 { ev=-14.48, d=43, pd=Rg6, mt=00:02:16, tl=00:03:34, s=13591 kN/s, n=1847437481, pv=Nd4 Rg6 Kh8 Rb6 Ra8 Bg6 Rf8 b4 Nf3 Kh5 Rd8 Re6 Nd4 Re8 Rxe8 Bxe8 Ne6 Bg6 Nc7 Bf7 Nb5 Kg5 Nd6 Bg6 Nb5, tb=0, R50=49, wv=14.48,  }  
77.Rg6+ { ev=4.84, d=32, pd=Kh8, mt=00:02:41, tl=00:23:44, s=31007 kN/s, n=5030257930, pv=Rg6 Kh8 Rb6 Rf8 b4 Nf3 Kg6 Nd2 Ba2 Nf3 b5 Ne5 Kg5 Nf3 Kh5 Rf5 Kg4 Rf8 Bb1 Nd2 Bf5 Nf1 Bg6 Ne3 Kg5, tb=0, R50=49, wv=4.84,  }  
Kh8 { ev=-14.48, d=45, pd=Rb6, mt=00:00:10, tl=00:03:55, s=9592 kN/s, n=102359746, pv=Kh8 Rb6 Ra8 Bg6 Rf8 b4 Nf3 Kh5 Rd8 Re6 Nd4 Re8 Rxe8 Bxe8 Ne6 Bg6 Nc7 Bf7 Nb5 Kg5 Nd6 Bg6 Nb5 Kf5 Nd4, tb=0, R50=48, wv=14.48,  }  
78.Rb6 { ev=5.10, d=29, pd=Nf3, mt=00:00:32, tl=00:23:42, s=31601 kN/s, n=1036245727, pv=Rb6 Nf3 Kf6 Nd2 Ke7 Ra8 Bd3 Rc8 b4 Nc4 Rb7 Ne3 b5 Nd5 Kf7 Rg8 Bg6 Ne3 Kf6 Rf8 Ke7 Rg8 Be4 Nc4 Kf6, tb=0, R50=48, wv=5.10,  }  
Ra8 { ev=-18.42, d=46, pd=Rd6, mt=00:01:43, tl=00:02:42, s=12109 kN/s, n=1252881054, pv=Ra8 Rd6 Rg8 Bg6 Nb3 Rd7 Ra8 Bf5 Rg8 Kf6 Ra8 Rh7 Kg8 Rg7 Kh8 Rd7 Rb8 Ke7 Nc5 Rd8 Rxd8 Kxd8 Na4 b4 Nc3, tb=0, R50=47, wv=18.42,  }  
79.b4 { ev=6.10, d=28, pd=Nf3, mt=00:00:42, tl=00:23:30, s=32164 kN/s, n=1385855432, pv=b4 Nf3 Kf6 Nd2 Bg6 Rg8 b5 Nc4 Rb7 Rd8 Ke7 Rg8 Bf5 Ne3 Be4 Rc8 Rd7 Nc4 Rd8 Rxd8 Kxd8 Na5 b6 Kg8 Ke7, tb=0, R50=50, wv=6.10,  }  
Kg8 { ev=-18.52, d=33, pd=Rd6, mt=00:00:09, tl=00:03:03, s=6124 kN/s, n=56763876, pv=Kg8 Rd6 Nf3 Kh5 Re8 Rd7 Rb8 Bf5 Ne5 Rc7 Nf3 Be4 Nd4 Rg7 Kh8 Rh7 Kg8 Rd7 Ne2 Kg5 Nc3 Bh7 Kh8 Bf5 Nb5, tb=0, R50=49, wv=18.52,  }  
80.Rb7 { ev=7.40, d=24, pd=Nf3, mt=00:00:24, tl=00:23:37, s=31904 kN/s, n=786378088, pv=Rb7 Nf3 Kh5 Nd4 b5 Ne6 Bf5 Nd4 Be4 Rd8 b6 Ne6 Re7 Nc5 Bf5 Kh8 Rh7 Kg8 Rg7 Kh8 b7 Nxb7 Rxb7 Rf8 Kg5, tb=0, R50=49, wv=7.40,  }  
Ne6+ { ev=-76.96, d=35, pd=Kf6, mt=00:00:25, tl=00:03:08, s=7017 kN/s, n=181522471, pv=Ne6 Kf6 Nd4 b5 Rd8 Ba2 Kh8 Kg5 Nf3 Kh5 Re8 Bb1 Nd4 b6 Re5 Kg4 Re8 Bg6 Rf8 Rh7 Kg8 Rc7 Nb5 Rg7 Kh8, tb=0, R50=48, wv=76.96,  }  
81.Kf6 { ev=9.23, d=24, pd=Nd8, mt=00:00:20, tl=00:23:47, s=34878 kN/s, n=729105782, pv=Kf6 Nd8 Rb6 Ra1 Bf5 Ra8 Ke7 Kh8 Bg6 Kg8 Be4 Rc8 Rd6 Nb7 Bxb7 Rc7 Rd7 Rxd7 Kxd7 Kh7 Be4 Kxh6 Ke7 Kg5 b5, tb=0, R50=48, wv=9.23,  }  
Nf8 { ev=-133.68, d=38, pd=b5, mt=00:01:55, tl=00:01:42, s=10792 kN/s, n=3001098897, pv=Nf8 b5 Rd8 Ba2 Kh8 b6 Rd6 Ke7 Rxh6 Kxf8 Rc6 Rb8 Kh7 Ke7 Kg6 Be6 Kg5 b7 Rb6 Rg8 Kf4 b8=Q Rxb8 Rxb8 Ke3, tb=0, R50=47, wv=133.68,  }  
82.b5 { ev=#21, d=24, pd=Rd8, mt=00:00:52, tl=00:23:25, s=30626 kN/s, n=1604808840, pv=b5 Rd8 Rg7 Kh8 b6 Nd7 Kg6 Ne5 Kh5 Nc6 Rc7 Na5 b7 Rd5 Kg6 Rd6 Kg5 Rd8 Bg6 Nxb7 Rxb7 Rd5 Bf5 Rd8 Rh7, tb=0, R50=50, wv=#21,  }  
Rd8 { ev=-135.90, d=37, pd=Ba2, mt=00:01:13, tl=00:00:59, s=7185 kN/s, n=2277060548, pv=Rd8 Ba2 Kh8 b6 Rd2 Rb8 Rf2 Ke7 Re2 Kxf8 Rxa2 Rc8 Rb2 Ke7 Kh7 Rc6 Kg8 Kd6 Kf7 Kc7 Re2 h7 Re7 Kb8 Kg7, tb=0, R50=49, wv=135.90,  }  
83.Rg7+ { ev=#20, d=22, pd=Kh8, mt=00:00:12, tl=00:23:43, s=22775 kN/s, n=287789565, pv=Rg7 Kh8 b6 Nd7 Kg6 Ne5 Kh5 Nc6 Rh7 Kg8 Rc7 Rd5 Kh4 Rd1 Bh7 Kf8 Be4 Ne7 h7 Kf7 h8=Q Ke6 Rxe7 Kxe7 b7, tb=0, R50=49, wv=#20,  }  
1-0 
; { Game Nr. 117 : Komodo 1092 wins against Stockfish 160913 by GUI adjudication 
; GameDuration = 04:54:28, Finalposition 3r1nk1/6R1/5K1P/1P6/8/8/8/1B6 b - - 2 83 }
"""

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

val android = """[Event "AI Factory's Chess"]
[Site "Android Device"]
[Date "2014.04.23"]
[Round "1"]
[White "You"]
[Black "Cpu (3)"]
[PlyCount "69"]
[Result "1-0"]

1. e2e4 e7e5 2. Qd1f3 Ng8f6 3. Bf1c4 Nb8c6 4. Nb1c3 h7h6 5. Qf3d3 d7d6
6. Ng1f3 a7a6 7. Nf3xe5 Rh8g8 8. Ne5xf7 Bc8d7 9. Nf7xd8 Rg8h8 10. Nd8e6 Bd7xe6
11. Bc4xe6 Bf8e7 12. Nc3d5 Ke8d8 13. Nd5xe7 Kd8xe7 14. Qd3c4 b7b5 15. Qc4xc6 Ke7xe6
16. d2d3 Ra8c8 17. Bc1f4 Ke6e7 18. e4e5 Nf6e8 19. Qc6xa6 Rh8f8 20. Bf4g3 Rc8b8
21. Qa6a7 Rb8d8 22. Bg3h4+ g7g5 23. Bh4g3 Rf8f7 24. Qa7a6 b5b4 25. h2h4 b4b3
26. a2xb3 Rf7g7 27. Qa6b5 Rd8d7 28. Ra1a8 Rg7f7 29. Qb5b4 Rf7f8 30. Ke1d1 Rf8f5
31. Rh1e1 Rd7d8 32. e5xd6+ Ke7d7 33. Qb4a4+ Rf5b5 34. Qa4xb5+ c7c6 35. Qb5f5# 1-0"""

val chesskids = """[Site "ChessKid iPhone"]
[Date "04/29/2014 02:27PM"]
[White "NicePlatypus"]
[Black "Computer"]

1. e4 e5 2. Nc3 Nc6 3. Nf3 Bc5 4. Bb5 d6 5. Bxc6+ bxc6 6. d4 exd4 7. Nxd4 Ne7 8. Be3 Bb4 9. a3 Bxc3+ 10. bxc3 c5 11. Nf3 f5 12. exf5 Bxf5
13. 0-0 0-0 14. Bg5 c6 15. Rb1 Be4 16. Rb7 Re8 17. Re1 d5 18. Ne5 Qd6 19. f4 Qe6 20. Bxe7 Rxe7 21. Rxe7 Qxe7 22. Nxc6 Qe6 23. Ne5 Rf8 24. g3
Rb8 25. c4 Rf8 26. cxd5 Qxd5 27. Qxd5+ Bxd5 28. Nd3 c4 29. Nb4 Bf7 30. c3 Re8 31. Rxe8+ Bxe8 32. Kf2 Kf7 33. Ke3 Bb5 34. Ke4 Bd7 35. Ke5 a5
36. Nd5 Bc6 37. Nb6 Bb5 38. h4 Ba6 39. g4 h5 40. gxh5 a4 41. Kf5 Bb5 42. Kg5 Ke7 43. h6 gxh6+ 44. Kxh6 Kd6 45. f5 Kc5 46. f6 Be8 47. Nxa4+
Kd5 48. Nb6+ Ke6 49. Kg7 Bh5 50. a4 Be8 51. a5 Bh5 52. a6 Ke5 53. f7 Bxf7 54. Kxf7 Ke4 55. a7 Kd3 56. a8Q Kxc3 57. Qa5+ Kd3 58. Qd5+ Ke2 59.
h5 c3 60. Qc4+ Kd2 61. Na4 c2 62. Qc3+ Ke2 63. Qxc2+ Ke1 64. Kf6 Kf1 65. Kf5 Ke1 66. Kf4 Kf1 67. Kf3 Ke1 68. Qe2# 1-0"""

val variations = """
1. e4 d6 2. d4 c6  3. f4  Qc7 4. Nf3 h6 $6  5. Bd3 (  5. Bc4  e6) 5... Bg4 6. O-O Nd7  7. Nc3 a6 $6  8. Be3 e5 $2  9. fxe5 dxe5 10. d5 $1 ( 10. dxe5 $2 Nxe5 ) 10... c5 (10... cxd5 $6 11. Nxd5 ) ( 10... Ngf6 11. dxc6 ) 
"""
}
