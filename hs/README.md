# [Advent of Code 2023](https://adventofcode.com/2023)
### my answers in [Haskell](https://www.haskell.org/)

This project builds with [The Haskell Cabal](https://www.haskell.org/cabal/).

Setup:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install cabal latest
ghcup install ghc 9.4.7
cabal configure --with-compiler ghc-9.4 --enable-tests
```

Run the [Hspec](https://hspec.github.io/) test suite:

```sh
cabal test test:aoc2023-test
```

Run [criterion](http://www.serpentine.com/criterion/) benchmarks ([results online](https://ephemient.github.io/aoc2023/aoc2023-bench.html)):

```sh
cabal bench bench:aoc2023-bench
```

Print solutions for the inputs provided in local data files:

```sh
cabal run exe:aoc2023
```

Generate [Haddock](https://www.haskell.org/haddock/) API documentation:

```sh
cabal haddock lib:aoc2023
```

Run [hlint](https://github.com/ndmitchell/hlint) source code suggestions:

```sh
cabal install hlint
hlint src test bench
```
