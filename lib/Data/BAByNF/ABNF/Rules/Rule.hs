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
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "rule")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition $
    Model.Elements . Model.Alternation  . List.singleton . Model.Concatenation $
        [ Model.Repetition Model.NoRepeat (Model.RulenameElement Rulename.ref)
        , Model.Repetition Model.NoRepeat (Model.RulenameElement DefinedAs.ref)
        , Model.Repetition Model.NoRepeat (Model.RulenameElement Elements.ref)
        , Model.Repetition Model.NoRepeat (Model.RulenameElement CNl.ref)
        ]

fromTree :: Tree Model.Rulename -> Either String Model.Rule
fromTree tree =
    name >>= \name' ->
    definedAs >>= \definedAs' ->
    elements >>= \elements' ->
    return $ Model.Rule name' definedAs' elements'
    where name = Tree.tryGetChildWithRef Rulename.ref tree <&> Rulename.fromTree
          definedAs = Tree.tryGetChildWithRef DefinedAs.ref tree >>= DefinedAs.fromTree
          elements = Tree.tryGetChildWithRef Elements.ref tree >>= Elements.fromTree
