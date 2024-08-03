module Data.BAByNF.ABNF.Rules.Element
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))
import Data.List qualified as List

import Data.BAByNF.ABNF.Model qualified as Model
import {-# SOURCE #-} Data.BAByNF.ABNF.Rules.Option qualified as Option
import Data.BAByNF.ABNF.Rules.CharVal qualified as CharVal
import Data.BAByNF.ABNF.Rules.NumVal qualified as NumVal
import Data.BAByNF.ABNF.Rules.Rulename qualified as Rulename
import Data.BAByNF.ABNF.Rules.Group qualified as Group
import Data.BAByNF.ABNF.Rules.ProseVal qualified as ProseVal
import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.Core.Ref qualified as Ref


ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe  "element")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition 
    . Model.Elements 
    . Model.Alternation
    $ 
        [ Model.Concatenation 
            . List.singleton
            . Model.Repetition Model.NoRepeat
            $ Model.RulenameElement Rulename.ref
        , Model.Concatenation 
            . List.singleton
            . Model.Repetition Model.NoRepeat
            $ Model.RulenameElement Group.ref
        , Model.Concatenation 
            . List.singleton
            . Model.Repetition Model.NoRepeat
            $ Model.RulenameElement Option.ref
        , Model.Concatenation 
            . List.singleton
            . Model.Repetition Model.NoRepeat
            $ Model.RulenameElement CharVal.ref
        , Model.Concatenation 
            . List.singleton
            . Model.Repetition Model.NoRepeat
            $ Model.RulenameElement NumVal.ref
        , Model.Concatenation 
            . List.singleton
            . Model.Repetition Model.NoRepeat
            $ Model.RulenameElement ProseVal.ref
        ]

fromTree :: Tree Model.Rulename -> Either String Model.Element
fromTree tree = 
    case Tree.nodes tree of
        [Tree.RefNode r subtree] -> 
            if Ref.eq r Rulename.ref then Right . Model.RulenameElement . Rulename.fromTree $ subtree  
            else if Ref.eq r Group.ref then Group.fromTree subtree <&> Model.GroupElement
            else if Ref.eq r Option.ref then Option.fromTree subtree <&> Model.OptionElement
            else if Ref.eq r CharVal.ref then CharVal.fromTree subtree <&> Model.CharValElement
            else if Ref.eq r NumVal.ref then NumVal.fromTree subtree <&> Model.NumValElement
            else if Ref.eq r ProseVal.ref then ProseVal.fromTree subtree <&> Model.ProseValElement
            else Left "element must be rulename | group | option | char-val | num-val | prose-val"
        _ -> Left "structural mismatch for <element>"
