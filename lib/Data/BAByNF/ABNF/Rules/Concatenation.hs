{-# LANGUAGE LambdaCase #-}
module Data.BAByNF.ABNF.Rules.Concatenation
    ( ref
    , rule
    , fromTree
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF.Rules.Repetition qualified as Repetition
import Data.BAByNF.ABNF.Rules.CWsp qualified as CWsp
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "concatenation")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition $ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    $ 
        [ Model.Repetition Model.NoRepeat (Model.RulenameElement Repetition.ref)
        , Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) 
            . Model.GroupElement
            . Model.Group
            . Model.Alternation
            . List.singleton
            . Model.Concatenation
            $
                [ Model.Repetition (Model.RangedRepeat (Model.FixedBound 1) Model.UnBound) (Model.RulenameElement CWsp.ref) 
                , Model.Repetition Model.NoRepeat (Model.RulenameElement Repetition.ref)
                ]
        ]

fromTree :: Tree Model.Rulename -> Either String Model.Concatenation
fromTree tree = mapM Repetition.fromTree 
    ( 
        Tree.getChildrenWithRef Repetition.ref tree
    ) >>= \case 
        [] -> Left "empty concat"
        x -> Right $ Model.Concatenation x
