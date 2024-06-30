module Data.BAByNF.ABNF.Rules.ProseVal
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))

import Data.Maybe qualified as Maybe
import Data.List qualified as List

import Data.ByteString qualified as ByteString

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.Hex qualified as Hex

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "prose-val")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition 
    . ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    $ 
        [ ABNF.Repetition ABNF.NoRepeat 
            . ABNF.CharValElement
            . ABNF.CaseInsensitiveCharVal
            . ABNF.CaseInsensitiveString
            . ABNF.QuotedString
            $ Ascii.stringAsBytesUnsafe "<"
        , ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound) 
            . ABNF.GroupElement
            . ABNF.Group
            . ABNF.Alternation
            $ 
                [ ABNF.Concatenation
                    . List.singleton
                    . ABNF.Repetition ABNF.NoRepeat
                    . ABNF.NumValElement
                    . ABNF.HexNumVal
                    $ ABNF.RangeHexVal (Hex.Seq [Hex.X2, Hex.X0]) (Hex.Seq [Hex.X3, Hex.XD])
                , ABNF.Concatenation
                    . List.singleton
                    . ABNF.Repetition ABNF.NoRepeat
                    . ABNF.NumValElement
                    . ABNF.HexNumVal
                    $ ABNF.RangeHexVal (Hex.Seq [Hex.X3, Hex.XF]) (Hex.Seq [Hex.X7, Hex.XE])
                ]
        , ABNF.Repetition ABNF.NoRepeat 
            . ABNF.CharValElement
            . ABNF.CaseInsensitiveCharVal
            . ABNF.CaseInsensitiveString
            . ABNF.QuotedString
            $ Ascii.stringAsBytesUnsafe ">"
        
        ] 

fromTree :: Tree ABNF.Rulename -> Either String ABNF.ProseVal
fromTree tree =
    let whole = Tree.stringify tree
        proseOrErr = Maybe.fromMaybe (Left "prose must be between < and >") $ ByteString.uncons whole >>= \(h, t) ->
             ByteString.unsnoc t <&> \(prose', l) ->
                if h == 60 && l == 62 then Right prose' else Left "prose must be between < and >"
     in proseOrErr <&> ABNF.ProseVal