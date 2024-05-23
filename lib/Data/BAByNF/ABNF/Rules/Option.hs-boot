module Data.BAByNF.ABNF.Rules.Option
    ( ref
    , rule
    , fromTree
    ) where

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.ABNF qualified as ABNF

ref :: ABNF.Rulename
rule :: ABNF.Rule
fromTree :: Tree ABNF.Rule -> Either String ABNF.Option

