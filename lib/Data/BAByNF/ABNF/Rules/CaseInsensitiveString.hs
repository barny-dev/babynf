module Data.BAByNF.ABNF.Rules.CaseInsensitiveString
    ( ref
    , rule
    , fromTree
    ) where

import Data.List qualified as List
import Data.Functor ( (<&>) )

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.ABNF.Rules.QuotedString qualified as QuotedString
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "case-insensitive-string")

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
            . Model.CharValElement
            . Model.CaseInsensitiveCharVal
            . Model.CaseInsensitiveString
            . Model.QuotedString
            $ Ascii.stringAsBytesUnsafe "%i"
        , Model.Repetition Model.NoRepeat
            . Model.RulenameElement
            $ QuotedString.ref
        ]

fromTree :: Tree Model.Rulename -> Either String Model.CaseInsensitiveString
fromTree tree = maybe (Left "no quoted-string") 
    QuotedString.fromTree (Tree.getChildWithRef QuotedString.ref tree) 
    <&> Model.CaseInsensitiveString