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
import Data.BAByNF.ABNF.Rules.CaseInsensitiveString qualified as CaseInsensitiveString
import Data.BAByNF.ABNF.Rules.CaseSensitiveString qualified as CaseSensitiveString
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "char-val")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition
    . Model.Elements
    . Model.Alternation
    $
        [ Model.Concatenation . List.singleton . Model.Repetition Model.NoRepeat $ Model.RulenameElement CaseInsensitiveString.ref
        , Model.Concatenation . List.singleton . Model.Repetition Model.NoRepeat $ Model.RulenameElement CaseSensitiveString.ref
        ]

fromTree :: Tree Model.Rulename -> Either String Model.CharVal
fromTree tree =
    fromMaybe (Left "no string") $ tryGetInsensitive tree <|> tryGetSensitive tree
    where tryGetInsensitive t = do
            subtree <- Tree.getChildWithRef CaseInsensitiveString.ref t
            return $ CaseInsensitiveString.fromTree subtree <&> Model.CaseInsensitiveCharVal
          tryGetSensitive t = do
            subtree <- Tree.getChildWithRef CaseSensitiveString.ref t
            return $ CaseSensitiveString.fromTree subtree <&> Model.CaseSensitiveCharVal