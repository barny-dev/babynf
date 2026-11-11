# Development and compatibility checks

Use GHC 9.2 or newer and Cabal 3.16.1.0 for the CI-equivalent commands:

```sh
cabal update
cabal build all --enable-tests --enable-benchmarks
cabal test all --enable-tests --test-show-details=direct --test-options='--timeout 30s --num-threads 2'
cabal check
cabal haddock lib:babynf
cabal bench babynf-bench --enable-benchmarks --benchmark-options='--timeout 0.1 --csv baseline.csv +RTS -T -RTS'
```

Increase the benchmark timeout for more stable local measurements and compare on
the same hardware/compiler. The checks deliberately include ten expected-failure
cases documenting existing defects; see [parser-contract.md](parser-contract.md).

## CI matrix

Linux jobs use GHC 9.2.8, 9.4.8, 9.6.7, 9.8.4, 9.10.3, 9.12.4, and 9.14.1.
macOS and Windows jobs use 9.12.4. Old compiler jobs use Ubuntu 22.04 to retain
compatible bindist system libraries. Every matrix job builds the library, tests,
and benchmark component, then runs tests. Cabal and action revisions are pinned.

The workflow runs on pushes, pull requests, manual dispatch, and weekly against
the current Hackage index. Solver plans, freeze files, toolchain information, and
logs are retained for fourteen days. Dependency caches are separated by OS,
architecture, compiler, Cabal, and solver plan. `tested-with` is updated only after
successful verification; declared bounds and the CI matrix are not proof of a
completed run.

The lower-bound job tests the oldest declared library dependencies together on
GHC 9.2.8. Test tools use compatible versions, not artificially minimal versions:

```sh
cabal build all --enable-tests --constraint='bytestring == 0.11.1.0' --constraint='attoparsec == 0.14.2'
cabal test all --enable-tests --constraint='bytestring == 0.11.1.0' --constraint='attoparsec == 0.14.2' --test-show-details=direct
```

The package job creates a source archive, then builds the library, tests, and
benchmarks and runs tests/Haddock from the extracted archive. This catches missing
packaged files. It also records a short benchmark baseline; no timing threshold
controls whether a change passes.

## Reproducing a run

Download its `results-...` artifact and use the recorded compiler and Cabal
versions. Copy its `cabal.project.freeze` to a clean checkout at the run's commit;
it records selected dependencies and the index state. The matching `plan.json`
helps identify transitive selections. Freeze files are intentionally not
committed as a universal dependency plan for all compilers.

No release upload, branch protection change, or deployment is performed by CI.
