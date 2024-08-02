module Data.BAByNF.ABNF.Rules.Rulelist
    ( ref
    , rule
    , fromTree
    ) where



import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Rules.CWsp qualified as CWsp
import Data.BAByNF.ABNF.Rules.CNl qualified as CNl
import Data.BAByNF.ABNF.Rules.Rule qualified as Rule

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "rulelist")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition $
    ABNF.Elements . ABNF.Alternation . List.singleton . ABNF.Concatenation . List.singleton .
        ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 1) ABNF.UnBound) .
        ABNF.GroupElement . ABNF.Group . ABNF.Alternation $
            [ ABNF.Concatenation . List.singleton . ABNF.Repetition ABNF.NoRepeat . ABNF.RulenameElement $
                Rule.ref 
            , ABNF.Concatenation . List.singleton . ABNF.Repetition ABNF.NoRepeat .
              ABNF.GroupElement . ABNF.Group . ABNF.Alternation . List.singleton . ABNF.Concatenation $
                [ ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound) $ ABNF.RulenameElement CWsp.ref
                , ABNF.Repetition ABNF.NoRepeat $ ABNF.RulenameElement CNl.ref
                ]
            ]

fromTree :: Tree ABNF.Rulename -> Either [String] ABNF.Rulelist
fromTree tree =
    let errorsAndDecls = groupBySide $ map Rule.fromTree $
            Tree.getChildrenWithRef Rule.ref tree
     in case errorsAndDecls of
        (err:errors, _) -> Left $ err : errors
        ([], decls) -> Right $ ABNF.Rulelist decls
    where groupBySide :: [Either l r] -> ([l], [r])
          groupBySide = foldr (\lr (ls, rs) -> case lr of Left l -> (l:ls, rs); Right r -> (ls, r:rs)) ([], [])


