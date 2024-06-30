module Data.BAByNF.ABNF.Rules.Elements
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))
import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Rules.Alternation qualified as Alternation
import Data.BAByNF.ABNF.Rules.CWsp qualified as CWsp

import Data.BAByNF.ABNF qualified as ABNF


ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "elements")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition 
    . ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    $ 
        [ ABNF.Repetition ABNF.NoRepeat (ABNF.RulenameElement Alternation.ref)
        , ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound) (ABNF.RulenameElement CWsp.ref)
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.Elements
fromTree tree = Tree.tryGetChildWithRef Alternation.ref tree >>= Alternation.fromTree <&> ABNF.Elements


