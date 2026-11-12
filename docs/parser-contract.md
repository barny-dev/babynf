# Parser behavior and compatibility goals

## Supported execution behavior

- Inputs are bytes. Literal matching returns the original matched bytes.
- ABNF rule names and ordinary quoted literals ignore ASCII case. `%s` literals
  are case sensitive. Numeric terminals describe bytes, including NUL and 255.
- Sequence preserves fragment order. Rule references wrap their result in a
  named node; failed branches must not leave nodes in the successful tree.
- Alternatives are searched in declaration order, and the first **complete**
  successful derivation is returned as a single tree.
- An earlier alternative can be reconsidered when its continuation fails:
  `start = ("a" / "ab") "c"` accepts `abc`.
- Repetition is greedy but can give matches back to a suffix:
  `start = *"a" "a"` accepts `a`. Valid minimum and maximum counts are enforced.
- Finite repetition of an empty match terminates; unbounded empty matches are a
  known defect below. Consuming right recursion is supported.
- `Core.Parseable.toParser` and the public `ABNF.parse` require EOF. The exposed
  attoparsec parser accepts incremental chunks.
- Custom `Unit` actions can consume input and return named subtrees. Prose
  elements currently fail if reached; there is no prose resolver yet.

Grammar text uses CRLF line endings, including the final terminator.
`ABNF.parseRulelist` returns a semantic grammar model: comments are recognized
but discarded, and no source positions are retained. The reader does not perform
complete grammar validation. `ABNF.parse` does not implicitly add core-rule
definitions; applications must include them when required.

## Known limitations

| Topic | Current behavior |
| --- | --- |
| Duplicate basic definitions | Two `=` definitions are combined as alternatives; `DefinedAs` is ignored during dictionary construction. |
| Incremental definitions | A standalone `=/` definition is accepted without a base. |
| Undefined references | An undefined reference can be hidden by a successful alternative. |
| Numeric terminals | Numeric value 256 wraps to byte zero and matches NUL, in decimal, hexadecimal, and binary notation. |
| Repetition bounds | Reversed bounds can throw an exception. |
| Nullable repetition | Unbounded repetition of an optional expression can diverge. |
| Left recursion | Direct and indirect left recursion can diverge. |

Errors returned through the public API are unstructured strings.

## Compatibility goals

Align grammar validation and execution with established ABNF libraries, using
the RFCs for language semantics and concrete implementations for operational
behavior.

RFC 5234 [section 3.3](https://www.rfc-editor.org/rfc/rfc5234.html#section-3.3)
describes a basic definition followed by incremental additions. Its
[metagrammar](https://www.rfc-editor.org/rfc/rfc5234.html#section-4) accepts both
`=` and `=/` without enforcing relationships between definitions. The RFC does
not explicitly require textual-order validation; ordering checks are a library
policy. A basic definition with incremental additions is valid ABNF.

### Reference behavior

The comparison uses APG JavaScript 4.4.0 at revision
[`89b2173`](https://github.com/ldthomas/apg-js/tree/89b217377c10dfa73838387ebd11d69d347a8961)
(strict ABNF mode, full generation and attribute analysis) and Python `abnf` at
revision
[`abbc0e0`](https://github.com/declaresub/abnf/tree/abbc0e0afc87f67d69c167c3228de99a850397b7)
(pure Python backend, default alternation).

| Case | APG | Python `abnf` |
| --- | --- | --- |
| `start = "a"` then `start = "b"` | Rejects duplicate definition during generation. | Warns, replaces the first definition, and accepts only `b`. |
| `start =/ "a"` without a base | Rejects during generation. | Rejects while loading. |
| `start = missing / "a"`, input `a` | Rejects undefined reference during generation. | Loads; raises `GrammarError` during matching. |
| `%d256`, `%x100`, `%b100000000`, input NUL | Preserves value 256; rejects NUL and matches character U+0100. | Same result. |
| `start = 3*2"a"` | Rejects during generation. | Rejects while loading. |
| `start = *["a"]`, empty input or `a` | Accepts and terminates. | Accepts and terminates. |
| `start = start / "a"`, input `a` | Rejects during attribute analysis. | Loads; returns `ParseError` rather than supporting left recursion. |
| `start = other / "a"`, `other = start`, input `a` | Rejects during attribute analysis. | Loads; returns `ParseError`. |
| `start =/ "b"` before `start = "a"` | Rejects because the base is not previously defined. | Rejects for the same reason. |
| `start = "a"` then `start =/ "b"` | Accepts `a` and `b`. | Accepts `a` and `b`. |

These reference APIs support character value 256; BAByNF's byte backend has a
terminal domain of 0..255. Left recursion is a valid ABNF construct supported by
the GLL engine in
[`go-abnf`](https://github.com/pandatix/go-abnf/tree/f3977cbbbd8078c29d7d013c9db1fb5764714dfd),
which includes a
[test for cyclic derivations](https://github.com/pandatix/go-abnf/blob/f3977cbbbd8078c29d7d013c9db1fb5764714dfd/sppf_test.go#L86)
from a rule of the form `a = a / "x"`.

### Goals and policy decisions

- Preserve numeric values until backend range validation. The byte backend
  must report terminals outside 0..255 without modular wrapping. Cover decimal,
  hexadecimal, and binary forms independently.
- Report reversed repetition bounds and unresolved references as grammar errors,
  distinct from input mismatch. An alternative that happens to match must not
  conceal an unresolved reference in the assembled executable grammar.
- Support unbounded nullable repetition without divergence. `*["a"]` should
  accept empty input and strings of `a`; specify how empty iterations appear in
  the returned tree.
- Handle direct and indirect left recursion without divergence. Either support
  matching with an appropriate algorithm or diagnose it explicitly as an
  unsupported grammar feature, distinct from input mismatch.
- Distinguish basic definitions (`=`) from incremental additions (`=/`). APG's
  duplicate rejection and Python's warned replacement are different precedents;
  the duplicate policy remains to be selected. Both require a previous base for
  `=/`, making that a compatibility reference rather than an RFC ordering rule.
  The definition assembly policy must specify ordering, completeness, and how
  separately supplied grammar fragments are assembled.
- Add differential regression cases against pinned reference versions. Record
  grammar-loading errors separately from input mismatch and unsupported features.
  Include both orders of basic/incremental definitions, valid extensions, all
  three numeric forms, empty and consuming nullable repetitions, and direct and
  indirect recursion. Document differences arising from byte input, tree output,
  or chosen library policies.

Test maintenance, benchmark scenarios, and measurement instructions are described
in [development.md](development.md).
