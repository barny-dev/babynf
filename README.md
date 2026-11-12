# BAByNF

BAByNF reads ABNF grammars and uses them to parse strict `ByteString` input into
trees. It provides a grammar model, ABNF core rules, a grammar reader, and a
pretty printer. The current execution backend is attoparsec.

## Parse a word

Add `babynf` and `bytestring` to your component's `build-depends`. The following
program reads a grammar, adds the core rules needed by `ALPHA`, and parses input
using the `word` rule:

```haskell
module Main (main) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.BAByNF.ABNF.Core as Core
import qualified Data.BAByNF.ABNF.Model as Model
import qualified Data.BAByNF.ABNF.Parse as ABNF
import qualified Data.BAByNF.Core.Tree as Tree

main :: IO ()
main = do
    Model.Rulelist rules <- either fail pure $
        ABNF.parseRulelist (B8.pack "word = 1*ALPHA\r\n")
    tree <- either fail pure $
        ABNF.parse
            (Model.Rulelist (rules ++ Core.rules))
            (Model.Rulename (B8.pack "word"))
            (B8.pack "Hello")
    B8.putStrLn (Tree.stringify tree)
```

Output: `Hello`.

Grammar text uses CRLF line endings, including the final rule terminator.
`parseRulelist` reads syntax; it does not validate every reference or repetition
bound. `parse` requires the entire input to match and returns
`Either String (Tree Rulename)`. Add core rules explicitly when the application
grammar refers to names such as `ALPHA` or `DIGIT`.

## Behavior and limitations

- Rule names and ordinary quoted literals ignore ASCII case. `%s` literals are
  case sensitive. Numeric terminals match bytes.
- Grammar alternatives can be retried when a later expression fails. Greedy
  repetition can give matches back to its suffix.
- `Tree.StringNode` stores a matched byte fragment. `Tree.RefNode` stores a rule
  reference and its subtree. `Tree.stringify` concatenates the matched fragments.
- Comments are accepted in grammar text but not retained in `Model.Rulelist`.
  Pretty printing does not preserve comments or original formatting.
- Prose values (`<...>`) can be read into the grammar model. An executable prose
  element fails when reached; no custom prose resolver is provided yet.
- Errors are strings without source positions. Some grammars can also cause
  exceptions or nontermination, and numeric values above 255 can wrap to bytes.
  Read the [parser behavior and known limitations](docs/parser-contract.md)
  before executing untrusted grammars.

Planned improvements to grammar validation and execution are defined in the
[compatibility goals](docs/parser-contract.md#compatibility-goals), with APG and
Python `abnf` as references.

## Modules

| Module | Purpose |
| --- | --- |
| `Data.BAByNF.ABNF.Model` | Construct and inspect grammar values. |
| `Data.BAByNF.ABNF.Parse` | Read grammar text and parse application input. |
| `Data.BAByNF.ABNF.Core` | Standard ABNF core-rule definitions. |
| `Data.BAByNF.ABNF.PrettyPrint` | Render grammar values as ABNF. |
| `Data.BAByNF.Core.Tree` | Inspect, transform, and stringify parse trees. |
| `Data.BAByNF.Core.Parseable` | Build parser expressions and obtain an attoparsec parser. |

## Development

The compatibility target starts at GHC 9.0; CI covers the stable families through
9.14, plus macOS and Windows on GHC 9.12. See
[development.md](docs/development.md) for build commands, exact compiler versions,
test interpretation, benchmarks, and CI reproduction.
