module Data.BAByNF.ABNF.Rules.Repetition
    ( ref
    , rule
    , fromTree
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Rules.Repeat qualified as Repeat
import Data.BAByNF.ABNF.Rules.Element qualified as Element
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "repetition")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition 
    . Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    $ 
        [ Model.Repetition Model.NoRepeat 
            . Model.OptionElement
            . Model.Option
            . Model.Alternation
            . List.singleton
            . Model.Concatenation
            . List.singleton
            . Model.Repetition Model.NoRepeat
            $ Model.RulenameElement Repeat.ref
        , Model.Repetition Model.NoRepeat
            $ Model.RulenameElement Element.ref
        ]

fromTree :: Tree Model.Rulename -> Either String Model.Repetition
fromTree tree =
    let rd = maybe (Right Model.NoRepeat) Repeat.fromTree  (Tree.getChildWithRef Repeat.ref tree)
        e = Tree.tryGetChildWithRef Element.ref tree >>= Element.fromTree
     in rd >>= \rd' ->
         e >>= \e' -> return $ Model.Repetition rd' e'