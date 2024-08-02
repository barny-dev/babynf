module Data.BAByNF.ABNF.Rules.CharVal
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree

import Data.Maybe (fromMaybe)
import Data.List qualified as List

import Control.Applicative ((<|>))

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Rules.CaseInsensitiveString qualified as CaseInsensitiveString
import Data.BAByNF.ABNF.Rules.CaseSensitiveString qualified as CaseSensitiveString

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "char-val")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition
    . ABNF.Elements
    . ABNF.Alternation
    $
        [ ABNF.Concatenation . List.singleton . ABNF.Repetition ABNF.NoRepeat $ ABNF.RulenameElement CaseInsensitiveString.ref
        , ABNF.Concatenation . List.singleton . ABNF.Repetition ABNF.NoRepeat $ ABNF.RulenameElement CaseSensitiveString.ref
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.CharVal
fromTree tree =
    fromMaybe (Left "no string") $ tryGetInsensitive tree <|> tryGetSensitive tree
    where tryGetInsensitive t = do
            subtree <- Tree.getChildWithRef CaseInsensitiveString.ref t
            return $ CaseInsensitiveString.fromTree subtree <&> ABNF.CaseInsensitiveCharVal
          tryGetSensitive t = do
            subtree <- Tree.getChildWithRef CaseSensitiveString.ref t
            return $ CaseSensitiveString.fromTree subtree <&> ABNF.CaseSensitiveCharVal