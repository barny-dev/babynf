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
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "elements")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition 
    . Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    $ 
        [ Model.Repetition Model.NoRepeat (Model.RulenameElement Alternation.ref)
        , Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) (Model.RulenameElement CWsp.ref)
        ]

fromTree :: Tree Model.Rulename -> Either String Model.Elements
fromTree tree = Tree.tryGetChildWithRef Alternation.ref tree >>= Alternation.fromTree <&> Model.Elements


