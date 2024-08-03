# BAByNF

BAByNF is a library that is able to parse ABNF and generate parsers based on grammars defined in ABNF. Other formats may be supported in the future.

This is baby's first Haskell library. No need to be gentle.

## ABNF

This library provides a model for ABNF grammars (see module `Data.BAByNF.ABNF.Model`) and both core ABNF rules (see module `Data.BAByNF.ABNF.Core`) and ABNF rules for ABNF (see `Data.BAByNF.ABNF.Rules.*` modules). The `Data.BAByNF.ABNF.Parse` module provides methods to parse bytestrings into ABNF rule lists and to parse ABNF grammar structures (such as rules referenced by name) into tree-like structures. 

Prose support: TBD (currently fail when encountered)

Comments support: TBD (currently ignored during parsing)

## Tree

BAByNF parsers output tree-like structures defined in `Data.BAByNF.Core.Tree`. A tree is built of anonymous nodes that either contain a bytestring fragment or named nodes that contain a subtree. The later are used to represent output of parsing behind references (such as ABNF rule names).

