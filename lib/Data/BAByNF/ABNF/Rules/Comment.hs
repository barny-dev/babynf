module Data.BAByNF.ABNF.Rules.Comment
    ( ref
    , rule
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Core qualified as Core

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "comment")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition $ 
    ABNF.Elements . ABNF.Alternation . List.singleton . ABNF.Concatenation $
        [ ABNF.Repetition ABNF.NoRepeat $
          ABNF.CharValElement . ABNF.CaseInsensitiveCharVal . ABNF.CaseInsensitiveString . ABNF.QuotedString $
          Ascii.stringAsBytesUnsafe ";"
        , ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound) $
          ABNF.GroupElement . ABNF.Group . ABNF.Alternation $
            [ ABNF.Concatenation . List.singleton . ABNF.Repetition ABNF.NoRepeat . ABNF.RulenameElement $ Core.wspRef
            , ABNF.Concatenation . List.singleton . ABNF.Repetition ABNF.NoRepeat . ABNF.RulenameElement $ Core.vcharRef
            ]
        , ABNF.Repetition ABNF.NoRepeat $ 
          ABNF.RulenameElement Core.crlfRef
        ]
