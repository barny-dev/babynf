module Data.BAByNF.ABNF.Rules.CaseSensitiveString
    ( ref
    , rule
    , fromTree
    ) where

import Data.List qualified as List
import Data.Functor ((<&>))

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Rules.QuotedString qualified as QuotedString

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "case-sensitive-string")


rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition
    . ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    $
        [ ABNF.Repetition ABNF.NoRepeat
            . ABNF.CharValElement
            . ABNF.CaseInsensitiveCharVal
            . ABNF.CaseInsensitiveString
            . ABNF.QuotedString
            $ Ascii.stringAsBytesUnsafe "%s"
        , ABNF.Repetition ABNF.NoRepeat
            . ABNF.RulenameElement
            $ QuotedString.ref
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.CaseSensitiveString
fromTree tree = maybe (Left "no quoted-string") 
    QuotedString.fromTree (Tree.getChildWithRef QuotedString.ref tree) 
    <&> ABNF.CaseSensitiveString