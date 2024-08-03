module Data.BAByNF.ABNF.Rules.CNl
    ( ref
    , rule
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.ABNF.Rules.Comment qualified as Comment
import Data.BAByNF.ABNF.Model qualified as Model


ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "c-nl")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition $
    Model.Elements . Model.Alternation $
        [ Model.Concatenation . List.singleton . Model.Repetition Model.NoRepeat . Model.RulenameElement $ Comment.ref
        , Model.Concatenation . List.singleton . Model.Repetition Model.NoRepeat . Model.RulenameElement $ Core.crlfRef
        ]
