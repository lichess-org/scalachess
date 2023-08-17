#!/usr/bin/env python3

import io
import itertools
import os.path
import sys

try:
    import chess.pgn
except ImportError:
    print("Need python-chess: ", file=sys.stderr)
    print("$ pip3 install chess", file=sys.stderr)
    print(file=sys.stderr)
    raise


def sync(abcde, src, dst):
    f = open(dst, "w")

    print("package chess.opening", file=f)
    print(file=f)
    print("// Generated from https://github.com/lichess-org/chess-openings", file=f)
    print("// format: off", file=f)
    print(f"private[opening] def openingDbPart{abcde.upper()}: Vector[Opening] = Vector(", file=f)

    for line in itertools.islice(open(src), 1, None):
        eco, name, pgn = line.rstrip().split("\t")
        board = chess.pgn.read_game(io.StringIO(pgn), Visitor=chess.pgn.BoardBuilder)
        uci = " ".join(m.uci() for m in board.move_stack)
        print(f"""Opening("{eco}", "{name}", "{board.epd()}", "{uci}", "{pgn}"),""", file=f)

    print(")", file=f)


if __name__ == "__main__":
    for abcde in ["a", "b", "c", "d", "e"]:
        print(abcde)
        sync(
            abcde,
            os.path.join(os.path.dirname(__file__), "..", "chess-openings", f"{abcde}.tsv"),
            os.path.join(os.path.dirname(__file__), "src", "main", "scala", "opening", f"OpeningDbPart{abcde.upper()}.scala")
        )
