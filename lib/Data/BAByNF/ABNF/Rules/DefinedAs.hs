module Data.BAByNF.ABNF.Rules.DefinedAs
    ( ref
    , rule
    , fromTree
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.List qualified as Util.List
import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Rules.CWsp qualified as CWsp
import Data.BAByNF.ABNF qualified as ABNF

import Debug.Trace

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "defined-as")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition $ ABNF.Elements 
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation 
    $ [
        ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound) (ABNF.RulenameElement CWsp.ref),
        ABNF.Repetition ABNF.NoRepeat
            $ ABNF.GroupElement
            . ABNF.Group
            . ABNF.Alternation
            $ 
                [ ABNF.Concatenation
                    . List.singleton
                    . ABNF.Repetition ABNF.NoRepeat
                    . ABNF.CharValElement
                    . ABNF.CaseInsensitiveCharVal
                    . ABNF.CaseInsensitiveString
                    . ABNF.QuotedString
                    $ Ascii.stringAsBytesUnsafe "="
                , ABNF.Concatenation
                    . List.singleton
                    . ABNF.Repetition ABNF.NoRepeat
                    . ABNF.CharValElement
                    . ABNF.CaseInsensitiveCharVal
                    . ABNF.CaseInsensitiveString
                    . ABNF.QuotedString
                    $ Ascii.stringAsBytesUnsafe "=/"
                ],
        ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound) (ABNF.RulenameElement CWsp.ref)
    ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.DefinedAs
fromTree tree =
    let (_, mid, _) = Util.List.lrsplitWhenNot (Tree.nodes (trace (">>>>>1"++ show tree) tree)) isCWsp
     in case trace (">>>>>2 " ++ show mid) mid of
        [Tree.StringNode x] | x == Ascii.stringAsBytesUnsafe "=" -> Right ABNF.BasicDefinition
                            | x == Ascii.stringAsBytesUnsafe "=/" -> Right ABNF.IncrementalAlternative
                            | otherwise -> Left "DefinedAs must be \'=\' | \'=/\'"
        _ -> Left "structural mismatch for <defined-as>"
    where isCWsp node = Tree.isRefOf node CWsp.ref

