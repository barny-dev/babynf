# Parser behavior and regression baseline

This document records the current interpreter behavior that a replacement backend
must preserve unless an API change explicitly says otherwise. Known defects are
listed separately; they are not promises that invalid grammars remain accepted.

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
  `start = *"a" "a"` accepts `a`. Minimum and maximum counts are enforced.
- Finite repetition of an empty match terminates; unbounded empty matches are a
  known defect below. Consuming right recursion is supported.
- `Core.Parseable.toParser` and the public `ABNF.parse` require EOF. The exposed
  attoparsec parser accepts incremental chunks; tests compare every split of an
  overlapping-alternative example with complete-buffer parsing.
- Custom `Unit` actions can consume input and return named subtrees. Prose
  elements currently fail if reached; there is no prose resolver yet.

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

These establish validation and termination targets for the planned compiler and
native backend. They do not introduce grammar validation or a new error API.
Each probe runs in a child test process with a 64 MiB heap limit, an 8 MiB stack
limit, and a five-second parent timeout. A crash, resource exhaustion, timeout,
or acceptance is not a successful rejection. The normal regression suite is not
marked as expected failure.

## Performance baseline

`babynf-bench` covers HTTP and multiline SMTP input, flat repetition at increasing
sizes, overlapping alternatives whose suffix forces retries, consuming right
recursion, and late invalid input. Fixtures are checked before timings start.
Every successful output tree is traversed; measurements do not stop at the
outer `Either` constructor or include `Show` rendering.

Timings include input parsing and tree traversal. Grammar text is parsed before
measurement, so these results isolate the current execution path rather than
grammar-reading costs. Run the same scenarios and consumption contract after a
backend change. The CI CSV is a baseline artifact, without speed thresholds.
