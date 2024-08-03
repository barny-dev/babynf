module Data.BAByNF.ABNF.Rules.Comment
    ( ref
    , rule
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "comment")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition $ 
    Model.Elements . Model.Alternation . List.singleton . Model.Concatenation $
        [ Model.Repetition Model.NoRepeat $
          Model.CharValElement . Model.CaseInsensitiveCharVal . Model.CaseInsensitiveString . Model.QuotedString $
          Ascii.stringAsBytesUnsafe ";"
        , Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) $
          Model.GroupElement . Model.Group . Model.Alternation $
            [ Model.Concatenation . List.singleton . Model.Repetition Model.NoRepeat . Model.RulenameElement $ Core.wspRef
            , Model.Concatenation . List.singleton . Model.Repetition Model.NoRepeat . Model.RulenameElement $ Core.vcharRef
            ]
        , Model.Repetition Model.NoRepeat $ 
          Model.RulenameElement Core.crlfRef
        ]
