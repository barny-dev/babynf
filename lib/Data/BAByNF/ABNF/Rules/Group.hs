module Data.BAByNF.ABNF.Rules.Group
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))
import Data.List qualified as List
import Data.BAByNF.Core.Tree (Tree)
import {-# SOURCE #-} Data.BAByNF.ABNF.Rules.Alternation qualified as Alternation
import Data.BAByNF.ABNF.Rules.CWsp qualified as CWsp
import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.ABNF qualified as ABNF


ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "group")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition
    . ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    $
        [ ABNF.Repetition ABNF.NoRepeat 
            . ABNF.CharValElement
            . ABNF.CaseInsensitiveCharVal
            . ABNF.CaseInsensitiveString
            $ ABNF.QuotedString (Ascii.stringAsBytesUnsafe "(")
        , ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound) (ABNF.RulenameElement CWsp.ref)
        , ABNF.Repetition ABNF.NoRepeat (ABNF.RulenameElement Alternation.ref)
        , ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound) (ABNF.RulenameElement CWsp.ref)
        , ABNF.Repetition ABNF.NoRepeat 
            . ABNF.CharValElement
            . ABNF.CaseInsensitiveCharVal
            . ABNF.CaseInsensitiveString
            $ ABNF.QuotedString (Ascii.stringAsBytesUnsafe ")")
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.Group
fromTree tree = Alternation.fromTree tree <&> ABNF.Group

