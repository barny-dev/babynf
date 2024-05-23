module Data.BAByNF.ABNF.Rules.CWsp
    ( ref
    , rule
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.ABNF.Rules.CNl qualified as CNl

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "c-wsp")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition . ABNF.Elements . ABNF.Alternation $
    [ ABNF.Concatenation . List.singleton . ABNF.Repetition ABNF.NoRepeat . ABNF.RulenameElement $
      Core.wspRef
    , ABNF.Concatenation . List.singleton . ABNF.Repetition ABNF.NoRepeat . ABNF.GroupElement . ABNF.Group .
      ABNF.Alternation . List.singleton . ABNF.Concatenation $
        [ ABNF.Repetition ABNF.NoRepeat $ ABNF.RulenameElement CNl.ref 
        , ABNF.Repetition ABNF.NoRepeat $ ABNF.RulenameElement Core.wspRef
        ] 
    ]
