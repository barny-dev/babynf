module Data.BAByNF.ABNF.Rules.Rule
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))
import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Rules.Rulename qualified as Rulename
import Data.BAByNF.ABNF.Rules.DefinedAs qualified as DefinedAs
import Data.BAByNF.ABNF.Rules.Elements qualified as Elements
import Data.BAByNF.ABNF.Rules.CNl qualified as CNl

import Data.BAByNF.ABNF qualified as ABNF


ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "rule")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition $
    ABNF.Elements . ABNF.Alternation  . List.singleton . ABNF.Concatenation $
        [ ABNF.Repetition ABNF.NoRepeat (ABNF.RulenameElement Rulename.ref)
        , ABNF.Repetition ABNF.NoRepeat (ABNF.RulenameElement DefinedAs.ref)
        , ABNF.Repetition ABNF.NoRepeat (ABNF.RulenameElement Elements.ref)
        , ABNF.Repetition ABNF.NoRepeat (ABNF.RulenameElement CNl.ref)
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.Rule
fromTree tree =
    name >>= \name' ->
    definedAs >>= \definedAs' ->
    elements >>= \elements' ->
    return $ ABNF.Rule name' definedAs' elements'
    where name = Tree.tryGetChildWithRef Rulename.ref tree <&> Rulename.fromTree
          definedAs = Tree.tryGetChildWithRef DefinedAs.ref tree >>= DefinedAs.fromTree
          elements = Tree.tryGetChildWithRef Elements.ref tree >>= Elements.fromTree
