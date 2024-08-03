module Data.BAByNF.ABNF.Rules.Rulename
    ( ref
    , rule
    , fromTree
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "rulename")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition 
    (Model.Elements . Model.Alternation . List.singleton . Model.Concatenation $
        [ Model.Repetition Model.NoRepeat (Model.RulenameElement Core.alphaRef)
        , Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) $
            Model.GroupElement . Model.Group . Model.Alternation $
                [ Model.Concatenation . List.singleton $
                    Model.Repetition Model.NoRepeat $ Model.RulenameElement Core.alphaRef
                , Model.Concatenation . List.singleton $
                    Model.Repetition Model.NoRepeat $ Model.RulenameElement Core.digitRef
                , Model.Concatenation . List.singleton
                    $ Model.Repetition Model.NoRepeat $
                        Model.CharValElement . Model.CaseInsensitiveCharVal . Model.CaseInsensitiveString . Model.QuotedString $
                            Ascii.stringAsBytesUnsafe "-"
                ]
        
    ])

fromTree :: Tree Model.Rulename -> Model.Rulename
fromTree = Model.Rulename . Tree.stringify
