module Data.BAByNF.ABNF.Rules.Rulename
    ( ref
    , rule
    , fromTree
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.ABNF qualified as ABNF

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "rulename")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition 
    (ABNF.Elements . ABNF.Alternation . List.singleton . ABNF.Concatenation $
        [ ABNF.Repetition ABNF.NoRepeat (ABNF.RulenameElement Core.alphaRef)
        , ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 0) ABNF.UnBound) $
            ABNF.GroupElement . ABNF.Group . ABNF.Alternation $
                [ ABNF.Concatenation . List.singleton $
                    ABNF.Repetition ABNF.NoRepeat $ ABNF.RulenameElement Core.alphaRef
                , ABNF.Concatenation . List.singleton $
                    ABNF.Repetition ABNF.NoRepeat $ ABNF.RulenameElement Core.digitRef
                , ABNF.Concatenation . List.singleton
                    $ ABNF.Repetition ABNF.NoRepeat $
                        ABNF.CharValElement . ABNF.CaseInsensitiveCharVal . ABNF.CaseInsensitiveString . ABNF.QuotedString $
                            Ascii.stringAsBytesUnsafe "-"
                ]
        
    ])

fromTree :: Tree ABNF.Rulename -> ABNF.Rulename
fromTree = ABNF.Rulename . Tree.stringify
