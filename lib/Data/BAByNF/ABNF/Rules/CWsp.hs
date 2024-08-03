module Data.BAByNF.ABNF.Rules.CWsp
    ( ref
    , rule
    ) where

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.ABNF.Rules.CNl qualified as CNl
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "c-wsp")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition . Model.Elements . Model.Alternation $
    [ Model.Concatenation . List.singleton . Model.Repetition Model.NoRepeat . Model.RulenameElement $
      Core.wspRef
    , Model.Concatenation . List.singleton . Model.Repetition Model.NoRepeat . Model.GroupElement . Model.Group .
      Model.Alternation . List.singleton . Model.Concatenation $
        [ Model.Repetition Model.NoRepeat $ Model.RulenameElement CNl.ref 
        , Model.Repetition Model.NoRepeat $ Model.RulenameElement Core.wspRef
        ] 
    ]
