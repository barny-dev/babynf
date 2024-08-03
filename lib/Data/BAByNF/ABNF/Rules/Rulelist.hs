module Data.BAByNF.ABNF.Rules.Rulelist
    ( ref
    , rule
    , fromTree
    ) where



import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF.Rules.CWsp qualified as CWsp
import Data.BAByNF.ABNF.Rules.CNl qualified as CNl
import Data.BAByNF.ABNF.Rules.Rule qualified as Rule
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "rulelist")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition $
    Model.Elements . Model.Alternation . List.singleton . Model.Concatenation . List.singleton .
        Model.Repetition (Model.RangedRepeat (Model.FixedBound 1) Model.UnBound) .
        Model.GroupElement . Model.Group . Model.Alternation $
            [ Model.Concatenation . List.singleton . Model.Repetition Model.NoRepeat . Model.RulenameElement $
                Rule.ref 
            , Model.Concatenation . List.singleton . Model.Repetition Model.NoRepeat .
              Model.GroupElement . Model.Group . Model.Alternation . List.singleton . Model.Concatenation $
                [ Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) $ Model.RulenameElement CWsp.ref
                , Model.Repetition Model.NoRepeat $ Model.RulenameElement CNl.ref
                ]
            ]

fromTree :: Tree Model.Rulename -> Either [String] Model.Rulelist
fromTree tree =
    let errorsAndDecls = groupBySide $ map Rule.fromTree $
            Tree.getChildrenWithRef Rule.ref tree
     in case errorsAndDecls of
        (err:errors, _) -> Left $ err : errors
        ([], decls) -> Right $ Model.Rulelist decls
    where groupBySide :: [Either l r] -> ([l], [r])
          groupBySide = foldr (\lr (ls, rs) -> case lr of Left l -> (l:ls, rs); Right r -> (ls, r:rs)) ([], [])


