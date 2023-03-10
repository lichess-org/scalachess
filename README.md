[![Continuous Integration](https://github.com/ornicar/scalachess/actions/workflows/ci.yml/badge.svg)](https://github.com/ornicar/scalachess/actions/workflows/ci.yml)

Chess API written in scala for [lichess.org](https://lichess.org)

It is entirely functional, immutable, and free of side effects.

INSTALL
-------

Clone scalachess

    git clone git://github.com/ornicar/scalachess

Start [sbt](http://www.scala-sbt.org/download.html) in scalachess directory

    sbt

In the sbt shell, to compile scalachess, run

    compile

To run the tests

    test

To run benchmarks (takes more than 1 hour to finish to finish):

    bench / Jmh / run

Or to ouput a json file

    bench / Jmh / run -rf json

To run quick benchmarks (results may be inaccurate):

    bench / Jmh / run -i 1 -wi 1 -f1 -t1

Code formatting
---------------

This repository uses [scalafmt](https://scalameta.org/scalafmt/).

Please [install it for your code editor](https://scalameta.org/scalafmt/docs/installation.html)
if you're going to contribute to this project.

If you don't install it, please run `scalafmtAll` in the sbt console before committing.
