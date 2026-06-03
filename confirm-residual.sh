#!/usr/bin/env bash
# Run on the ISOLATED machine. Confirms whether the ~4.4% gap between the branch's
# `ceiling` hash mode and master.json is a real regression or just cross-run drift.
#
# Key fact: the branch's `ceiling` mode is a byte-faithful reconstruction of master's
# ENTIRE hashPosition (master's hCastling == 4 corner bit-tests == ceiling; everything
# else in hashPosition is identical between master and branch). So:
#   - if fresh master ~= hashcmp.json ceiling  -> master.json was stale; corner == master, DONE.
#   - if fresh master  >  ceiling (same gap)   -> a real build/inlining difference to chase.
#
# Produces master-fresh.json. Run from repo root. Same JMH knobs as the hashcmp run.
set -euo pipefail
cd "$(dirname "$0")"

ROOT="$(pwd)"
JMH='-i 10 -wi 8 -f 3'

echo ">>> stash any local changes, checkout master"
git stash -u || true
git checkout master

echo ">>> run master HashBench.hashes (fresh, same machine/session conditions)"
sbt "bench/Jmh/run $JMH -rf json -rff $ROOT/master-fresh.json benchmarks.HashBench.hashes"

echo ">>> back to branch"
git checkout castle-rights/0
git stash pop || true

echo
echo ">>> master-fresh vs hashcmp ceiling/corner:"
python3 - "$ROOT/master-fresh.json" "$ROOT/hashcmp.json" <<'PY'
import json, sys
def hashes(path):
    d = json.load(open(path))
    out = {}
    for e in d:
        if e["benchmark"].split(".")[-1] != "hashes": continue
        out[e.get("params", {}).get("castlingMode", "master")] = (
            e["primaryMetric"]["score"], e["primaryMetric"]["scoreError"])
    return out
mf = hashes(sys.argv[1]); cmp = hashes(sys.argv[2])
master = next(iter(mf.values()))
def overlap(a, b):
    return not (a[0]+a[1] < b[0]-b[1] or b[0]+b[1] < a[0]-a[1])
print(f'master-fresh : {master[0]:8.2f} ±{master[1]:.2f}')
for m in ("ceiling", "corner", "current"):
    if m in cmp:
        s, e = cmp[m]
        d = (s - master[0]) / master[0] * 100
        print(f'{m:<13}: {s:8.2f} ±{e:.2f}   vs master-fresh {d:+.2f}% '
              f'{"OVERLAP -> drift, no real regression" if overlap((s,e), master) else "DISJOINT -> real gap"}')
print()
print("If ceiling overlaps master-fresh: master.json was stale; ship corner fast-path == master.")
PY
