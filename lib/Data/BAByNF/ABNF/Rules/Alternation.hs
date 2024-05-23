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

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Rules.Concatenation qualified as Concatenation
import Data.BAByNF.ABNF.Rules.CWsp qualified as CWsp

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "alternation")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition 
    $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation 
    $ 
        [ ABNF.Repetition ABNF.NoRepeat (ABNF.RulenameElement Concatenation.ref)
        , ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound) 
            $ ABNF.GroupElement
            . ABNF.Group
            . ABNF.Alternation
            . List.singleton
            . ABNF.Concatenation
            $ 
                [ ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound) (ABNF.RulenameElement CWsp.ref)
                , ABNF.Repetition ABNF.NoRepeat 
                    $ ABNF.CharValElement 
                    . ABNF.CaseInsensitiveCharVal 
                    . ABNF.CaseInsensitiveString
                    . ABNF.QuotedString
                    $ Ascii.stringAsBytesUnsafe "/"
                , ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound) (ABNF.RulenameElement CWsp.ref)
                , ABNF.Repetition ABNF.NoRepeat (ABNF.RulenameElement Concatenation.ref)
                ]
      ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.Alternation
fromTree tree = 
    let concatTrees = Tree.getChildrenWithRef Concatenation.ref tree
     in mapM Concatenation.fromTree concatTrees >>=
            \case
                [] -> Left "empty alt"
                lst@(_:_) -> Right $ ABNF.Alternation lst
