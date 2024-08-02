module Data.BAByNF.ABNF.Rules.Alternation
    ( ref
    , rule
    , fromTree
    ) where

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.ABNF.Model qualified as Model


ref :: Model.Rulename
rule :: Model.Rule
fromTree :: Tree Model.Rulename -> Either String Model.Alternation
