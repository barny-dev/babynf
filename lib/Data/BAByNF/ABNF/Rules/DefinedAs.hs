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
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "defined-as")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition $ Model.Elements 
    . Model.Alternation
    . List.singleton
    . Model.Concatenation 
    $ [
        Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) (Model.RulenameElement CWsp.ref),
        Model.Repetition Model.NoRepeat
            $ Model.GroupElement
            . Model.Group
            . Model.Alternation
            $ 
                [ Model.Concatenation
                    . List.singleton
                    . Model.Repetition Model.NoRepeat
                    . Model.CharValElement
                    . Model.CaseInsensitiveCharVal
                    . Model.CaseInsensitiveString
                    . Model.QuotedString
                    $ Ascii.stringAsBytesUnsafe "="
                , Model.Concatenation
                    . List.singleton
                    . Model.Repetition Model.NoRepeat
                    . Model.CharValElement
                    . Model.CaseInsensitiveCharVal
                    . Model.CaseInsensitiveString
                    . Model.QuotedString
                    $ Ascii.stringAsBytesUnsafe "=/"
                ],
        Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) (Model.RulenameElement CWsp.ref)
    ]

fromTree :: Tree Model.Rulename -> Either String Model.DefinedAs
fromTree tree =
    let (_, mid, _) = Util.List.lrsplitWhenNot (Tree.nodes tree) isCWsp
     in case mid of
        [Tree.StringNode x] | x == Ascii.stringAsBytesUnsafe "=" -> Right Model.BasicDefinition
                            | x == Ascii.stringAsBytesUnsafe "=/" -> Right Model.IncrementalAlternative
                            | otherwise -> Left "DefinedAs must be \'=\' | \'=/\'"
        _ -> Left "structural mismatch for <defined-as>"
    where isCWsp node = Tree.isRefOf node CWsp.ref

