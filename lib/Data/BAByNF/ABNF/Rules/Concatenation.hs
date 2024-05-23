{-# LANGUAGE LambdaCase #-}
module Data.BAByNF.ABNF.Rules.Concatenation
    ( ref
    , rule
    , fromTree
    ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Grammar ((+!), (??))
import Data.BAByNF.ABNF.Grammar qualified as Grammar
import Data.BAByNF.ABNF.Rules.Repetition qualified as Repetition
import Data.BAByNF.ABNF.Rules.CWsp qualified as CWsp

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "concatenation")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    $ 
        [ ABNF.Repetition ABNF.NoRepeat (ABNF.RulenameElement Repetition.ref)
          ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound) 
            . ABNF.GroupElement
            . ABNF.Group
            . ABNF.Alternation
            . List.singleton
            . ABNF.Concatenation
            $
                [ ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 1) ABNF.UnBound) (ABNF.RulenameElement CWsp.ref) 
                , ABNF.Repetition ABNF.NoRepeat (ABNF.RulenameElement Repetition.ref)
                ]
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.Concatenation
fromTree tree = mapM Repetition.fromTree 
    ( 
        Tree.getChildrenWithRef Repetition.ref tree
    ) >>= \case 
        [] -> Left "empty concat"
        x -> Right $ ABNF.Concatenation x
