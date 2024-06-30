{-# LANGUAGE LambdaCase #-}
module Data.BAByNF.ABNF.Rules.Alternation
    ( ref
    , rule
    , fromTree
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF.Model qualified as Model
import Data.BAByNF.ABNF.Rules.Concatenation qualified as Concatenation
import Data.BAByNF.ABNF.Rules.CWsp qualified as CWsp

import Debug.Trace (trace)

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "alternation")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition 
    $ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation 
    $ 
        [ Model.Repetition Model.NoRepeat (Model.RulenameElement Concatenation.ref)
        , Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) 
            $ Model.GroupElement
            . Model.Group
            . Model.Alternation
            . List.singleton
            . Model.Concatenation
            $ 
                [ Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) (Model.RulenameElement CWsp.ref)
                , Model.Repetition Model.NoRepeat 
                    $ Model.CharValElement 
                    . Model.CaseInsensitiveCharVal 
                    . Model.CaseInsensitiveString
                    . Model.QuotedString
                    $ Ascii.stringAsBytesUnsafe "/"
                , Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) (Model.RulenameElement CWsp.ref)
                , Model.Repetition Model.NoRepeat (Model.RulenameElement Concatenation.ref)
                ]
      ]

fromTree :: Tree Model.Rulename -> Either String Model.Alternation
fromTree tree = 
    let concatTrees = Tree.getChildrenWithRef Concatenation.ref (trace (show tree) tree)
     in trace (show concatTrees) $ mapM Concatenation.fromTree concatTrees >>=
            \case
                [] -> Left "Alternation.hs: empty alt"
                lst@(_:_) -> Right $ Model.Alternation lst