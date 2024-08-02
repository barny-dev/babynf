{-# LANGUAGE LambdaCase #-}
module Data.BAByNF.ABNF.Rules.Option
    ( ref
    , rule
    , fromTree
    ) where

import Data.List qualified as List
import Data.Functor ((<&>))

import Data.BAByNF.Core.Ref qualified as Ref
import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Rules.Alternation qualified as Alternation
import Data.BAByNF.ABNF.Rules.CWsp qualified as CWsp
import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "option")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition
    . Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    $
        [ Model.Repetition Model.NoRepeat 
            . Model.CharValElement
            . Model.CaseInsensitiveCharVal
            . Model.CaseInsensitiveString
            $ Model.QuotedString (Ascii.stringAsBytesUnsafe "[")
        , Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) (Model.RulenameElement CWsp.ref)
        , Model.Repetition Model.NoRepeat (Model.RulenameElement Alternation.ref)
        , Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) (Model.RulenameElement CWsp.ref)
        , Model.Repetition Model.NoRepeat 
            . Model.CharValElement
            . Model.CaseInsensitiveCharVal
            . Model.CaseInsensitiveString
            $ Model.QuotedString (Ascii.stringAsBytesUnsafe "]")
        ]

fromTree :: Tree Model.Rulename -> Either String Model.Option
fromTree tree =
    let nodes = Tree.nodes tree
     in (tryDropLeftParens nodes
        >>= (\case
                (Tree.RefNode r subtree):rest
                     | Ref.eq Alternation.ref r -> Right (subtree, rest)
                     | otherwise -> Left "option must contain alternation"
                _ -> Left "structural mismatch for <option>"
            ) . dropCWsp)
        >>= \(altSubtree, rest) -> (tryDropRightParens . dropCWsp $ rest)
        >>= \case
            [] -> Alternation.fromTree altSubtree <&> Model.Option
            _ -> Left "structural mismatch for <option>"
    where tryDropLeftParens nodes =
            case nodes of
                (Tree.StringNode bs):rest | bs == Ascii.stringAsBytesUnsafe "[" -> Right rest
                                          | otherwise -> Left "structural mismatch for <option>"
                _ -> Left "structural mismatch for <option>"
          dropCWsp = dropWhile (`Tree.isRefOf` CWsp.ref)
          tryDropRightParens nodes =
            case nodes of
                (Tree.StringNode bs):rest | bs == Ascii.stringAsBytesUnsafe "]" -> Right rest
                                          | otherwise -> Left "structural mismatch for <group>"
                _ -> Left "structural mismatch for <group>"

