module Data.BAByNF.ABNF.Rules.Repetition
    ( ref
    , rule
    , fromTree
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Rules.Repeat qualified as Repeat
import Data.BAByNF.ABNF.Rules.Element qualified as Element

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "repetition")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition 
    . ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    $ 
        [ ABNF.Repetition ABNF.NoRepeat 
            . ABNF.OptionElement
            . ABNF.Option
            . ABNF.Alternation
            . List.singleton
            . ABNF.Concatenation
            . List.singleton
            . ABNF.Repetition ABNF.NoRepeat
            $ ABNF.RulenameElement Repeat.ref
        , ABNF.Repetition ABNF.NoRepeat
            $ ABNF.RulenameElement Element.ref
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.Repetition
fromTree tree =
    let rd = maybe (Right ABNF.NoRepeat) Repeat.fromTree  (Tree.getChildWithRef Repeat.ref tree)
        e = Tree.tryGetChildWithRef Element.ref tree >>= Element.fromTree
     in rd >>= \rd' ->
         e >>= \e' -> return $ ABNF.Repetition rd' e'