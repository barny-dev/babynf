# Parser behavior and regression baseline

This document describes the current parser behavior and known limitations.
The regression suite uses these behaviors as a baseline for backend changes.

## Supported execution behavior

- Inputs are bytes. Literal matching returns the original matched bytes.
- ABNF rule names and ordinary quoted literals ignore ASCII case. `%s` literals
  are case sensitive. Numeric terminals describe bytes, including NUL and 255.
- Sequence preserves fragment order. Rule references wrap their result in a
  named node; failed branches must not leave nodes in the successful tree.
- Alternatives are searched in declaration order, and the first **complete**
  successful derivation is returned. This is not a parse-forest API.
- An earlier alternative can be reconsidered when its continuation fails:
  `start = ("a" / "ab") "c"` accepts `abc`.
- Repetition is greedy but can give matches back to a suffix:
  `start = *"a" "a"` accepts `a`. Valid minimum and maximum counts are enforced.
- Finite repetition of an empty match terminates; unbounded empty matches are a
  known defect below. Consuming right recursion is supported.
- `Core.Parseable.toParser` and the public `ABNF.parse` require EOF. The exposed
  attoparsec parser accepts incremental chunks; tests compare every split of an
  overlapping-alternative example with complete-buffer parsing.
- Custom `Unit` actions can consume input and return named subtrees. Prose
  elements currently fail if reached; there is no prose resolver yet.

Grammar text uses CRLF line endings, including the final terminator.
`ABNF.parseRulelist` returns a semantic grammar model: comments are recognized
but discarded, and no source positions are retained. The reader does not perform
complete grammar validation. `ABNF.parse` does not implicitly add core-rule
definitions; applications must include them when required.

The core tests construct `Parseable` expressions directly, independently of the
ABNF syntax reader. Integration tests exercise grammar reading and protocol
fixtures. The HTTP request and SMTP EHLO fixtures are representative subsets,
not claims of full protocol conformance.

## Known defects and intended outcomes

The `known issues (expected failures)` group asserts the desired behavior with
`tasty-expected-failure`. Each case must currently fail. An unexpected pass fails
CI too, prompting removal of the marker when the implementation is fixed.

| Probe | Intended outcome |
| --- | --- |
| `duplicate-basic` | Reject multiple `=` definitions of one rule. |
| `orphan-incremental` | Reject `=/` without a basic definition. |
| `undefined-alternative` | Diagnose undefined references, including an unselected alternative. |
| `decimal-overflow`, `hex-overflow`, `binary-overflow` | Reject values outside the byte domain instead of wrapping. |
| `reversed-repeat` | Return a parser/grammar failure for reversed bounds, rather than an exception. |
| `nullable-repeat` | Terminate on an unbounded repetition of a nullable expression. |
| `direct-left-recursion`, `indirect-left-recursion` | Diagnose unsupported left recursion without divergence. |

These are validation and termination targets. The current implementation can
accept the invalid definitions or wrapped numeric values above; reversed bounds
can throw an exception, and nullable repetition or left recursion can diverge.
Errors returned through the public API are unstructured strings. A successful
grammar read is insufficient to establish that executing it is safe.
Each probe runs in a child test process with a 64 MiB heap limit, an 8 MiB stack
limit, and a five-second parent timeout. A crash, resource exhaustion, timeout,
or acceptance is not a successful rejection. The normal regression suite is not
marked as expected failure.

Benchmark scenarios and measurement instructions are described in
[development.md](development.md#benchmarks).
