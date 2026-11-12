# Development

## Build and test

Use a supported GHC on `PATH`. CI uses Cabal 3.16.1.0; the package description
uses the Cabal 3.4 format. From the repository root:

```sh
cabal update
cabal build all --enable-tests --enable-benchmarks
cabal test all --enable-tests --test-show-details=direct --test-options='--timeout 30s --num-threads 2'
cabal check
cabal haddock lib:babynf
```

Tests cover the core interpreter and ABNF integration, alongside rule/model/utility
tests. Core tests construct `Parseable` expressions independently of the grammar
reader. HTTP and SMTP fixtures are protocol subsets, not full conformance suites.

## Expected-failure tests

The `known issues (expected failures)` group uses `tasty-expected-failure` to
isolate known limitations. A failing assertion is reported as an expected
failure; an unexpected pass fails the suite so that obsolete markers are removed.

The probes assert normal `Left` rejection, including for nullable repetition,
whose [compatibility goal](parser-contract.md#compatibility-goals) is successful
matching with termination. When implementing a compatibility goal, update the
affected assertion to the specified result and remove its marker when the
implementation satisfies it.

Probes run in child processes with a 64 MiB heap limit, an 8 MiB stack limit, and
a five-second parent timeout. A crash, resource exhaustion, or timeout is not a
normal parser rejection. Harness controls check that a child can accept valid
input and reject invalid input.

## Compiler compatibility

The minimum family is GHC 9.0 (`base >= 4.15`). All components share the explicit
extensions in `babynf.cabal`, avoiding a requirement for the GHC2021 edition.

| Platform | Tested compiler patches |
| --- | --- |
| Linux | 9.0.2, 9.2.8, 9.4.8, 9.6.7, 9.8.4, 9.10.3, 9.12.4, 9.14.1 |
| macOS and Windows | 9.12.4 |

CI tests one patch per stable family, not every historical patch or every
compiler/OS combination. `tested-with` records verified patches. New compiler
families should pass CI before their `base` versions are admitted. Compiler
patches, Cabal, and action commits are pinned; runner labels ending in `-latest`
follow GitHub's OS images.

GHC 9.0, 9.2, and 9.4 jobs use Ubuntu 22.04 for older bindists; GHC 9.0 also
requires `libtinfo5`. Later Linux jobs use Ubuntu 24.04. Exact settings live in
`.github/workflows/ci.yml`.

## CI checks and artifacts

CI runs for pull requests, pushes to `master`/`develop`, manual dispatch, and
weekly against the current Hackage index. New runs cancel older runs for the
same PR number or branch ref. Separate PRs have separate concurrency groups,
including forks with identical branch names.

| Job | Checks | Downloadable artifact |
| --- | --- | --- |
| Compiler matrix | Build library, tests, and benchmarks; run tests. | `results-<runner-label>-ghc-<version>`: toolchain, solver plan, freeze file, build/test logs. |
| Library lower bounds | Build/test on GHC 9.0.2 with bytestring 0.11.1.0 and attoparsec 0.14.2. Test tools use compatible versions. | Use the Actions job log. |
| Package | Create an sdist; build/test and generate Haddock from its extracted sources; run benchmarks. | `package-and-baseline`: source archive, benchmark CSV, benchmark solver plan, GHC/Cabal versions. |

Artifacts are retained for fourteen days; save any baseline needed longer.
Dependency caches are separated by OS, architecture, compiler, Cabal, and solver
plan.

To run the library lower-bound check locally, select GHC 9.0.2 and run:

```sh
cabal build all --enable-tests --constraint='bytestring == 0.11.1.0' --constraint='attoparsec == 0.14.2'
cabal test all --enable-tests --constraint='bytestring == 0.11.1.0' --constraint='attoparsec == 0.14.2' --test-show-details=direct
```

The package job explicitly selects the `cabal.project` included in the sdist,
preventing accidental use of the parent checkout's project.

## Benchmarks

For a quick check of the thirteen scenarios:

```sh
cabal bench babynf-bench --enable-benchmarks --benchmark-options='--timeout 0.1 --csv baseline.csv +RTS -T -RTS'
```

For a longer measurement, replace `--timeout 0.1` with `--timeout 3`. Compare runs
on the same hardware, compiler, dependency plan, and optimization settings.
The short CI run records a CSV without speed thresholds; it is too noisy to
establish small performance differences.

Measurements include input parsing and traversal of every output tree node,
without adding `Show` rendering. Grammar text is parsed before measurement.
Cases cover protocol input, increasing flat repetition, overlapping alternatives,
consuming recursion, and late rejection.

## Reproduce a matrix run

1. Check out the commit identified by the Actions run.
2. Download its `results-...` artifact and select the recorded GHC/Cabal versions
   on the same OS and architecture.
3. Copy the artifact's `cabal.project.freeze` into the checkout root.
4. Run the build/test commands above. The freeze file records dependency versions
   and the Hackage index state; `plan.json` lists selected components.

Do not run `cabal freeze` again before reproduction, as that can replace recorded
selections. Local project overrides or a different system environment can still
change the build. Freeze files are not committed as one plan for all compilers.
