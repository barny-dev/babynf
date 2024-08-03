module Data.BAByNF.ABNF.Rules.ProseVal
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))

import Data.Maybe qualified as Maybe
import Data.List qualified as List

import Data.ByteString qualified as ByteString

import Data.BAByNF.ABNF.Model qualified as Model
import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.Hex qualified as Hex

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "prose-val")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition 
    . Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    $ 
        [ Model.Repetition Model.NoRepeat 
            . Model.CharValElement
            . Model.CaseInsensitiveCharVal
            . Model.CaseInsensitiveString
            . Model.QuotedString
            $ Ascii.stringAsBytesUnsafe "<"
        , Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) 
            . Model.GroupElement
            . Model.Group
            . Model.Alternation
            $ 
                [ Model.Concatenation
                    . List.singleton
                    . Model.Repetition Model.NoRepeat
                    . Model.NumValElement
                    . Model.HexNumVal
                    $ Model.RangeHexVal (Hex.Seq [Hex.X2, Hex.X0]) (Hex.Seq [Hex.X3, Hex.XD])
                , Model.Concatenation
                    . List.singleton
                    . Model.Repetition Model.NoRepeat
                    . Model.NumValElement
                    . Model.HexNumVal
                    $ Model.RangeHexVal (Hex.Seq [Hex.X3, Hex.XF]) (Hex.Seq [Hex.X7, Hex.XE])
                ]
        , Model.Repetition Model.NoRepeat 
            . Model.CharValElement
            . Model.CaseInsensitiveCharVal
            . Model.CaseInsensitiveString
            . Model.QuotedString
            $ Ascii.stringAsBytesUnsafe ">"
        
        ] 

fromTree :: Tree Model.Rulename -> Either String Model.ProseVal
fromTree tree =
    let whole = Tree.stringify tree
        proseOrErr = Maybe.fromMaybe (Left "prose must be between < and >") $ ByteString.uncons whole >>= \(h, t) ->
             ByteString.unsnoc t <&> \(prose', l) ->
                if h == 60 && l == 62 then Right prose' else Left "prose must be between < and >"
     in proseOrErr <&> Model.ProseVal