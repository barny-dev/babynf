module Data.BAByNF.ABNF.Rules.CNl
    ( ref
    , rule
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.ABNF.Rules.Comment qualified as Comment


ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "c-nl")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition $
    ABNF.Elements . ABNF.Alternation $
        [ ABNF.Concatenation . List.singleton . ABNF.Repetition ABNF.NoRepeat . ABNF.RulenameElement $ Comment.ref
        , ABNF.Concatenation . List.singleton . ABNF.Repetition ABNF.NoRepeat . ABNF.RulenameElement $ Core.crlfRef
        ]
