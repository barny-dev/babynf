module Data.BAByNF.ABNF.Rules.QuotedString
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))
import Data.Word (Word8)

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.Hex qualified as Hex

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "quoted-string")

rule :: Model.Rule
rule = Model.Rule ref  Model.BasicDefinition
    ( Model.Elements $ 
        Model.Alternation [Model.Concatenation 
            [ Model.Repetition Model.NoRepeat (Model.RulenameElement Core.dquoteRef)
            , Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound) (Model.GroupElement . Model.Group . Model.Alternation $
                    [ Model.Concatenation [ Model.Repetition Model.NoRepeat $ Model.NumValElement . Model.HexNumVal $ 
                        Model.RangeHexVal (Hex.Seq [Hex.X2, Hex.X0]) (Hex.Seq [Hex.X2, Hex.X1])]
                    , Model.Concatenation [ Model.Repetition Model.NoRepeat $ Model.NumValElement . Model.HexNumVal $
                        Model.RangeHexVal (Hex.Seq [Hex.X2, Hex.X3]) (Hex.Seq [Hex.X7, Hex.XE])]
                    ])
                
            , Model.Repetition Model.NoRepeat (Model.RulenameElement Core.dquoteRef)
            ]
        ]
    )

fromTree :: Tree Model.Rulename -> Either String Model.QuotedString
fromTree tree =
    maybe (Left "quoted-string must be between \" and \"") Right $
        unconsnoc (Tree.stringify tree) >>= \(h, m, l) ->
            if h == 34 && l == 34
                then Just (Model.QuotedString m)
                else Nothing

unconsnoc :: ByteString -> Maybe (Word8, ByteString, Word8)
unconsnoc bs = ByteString.uncons bs >>= \(h, t) -> ByteString.unsnoc t <&> \(m, l) -> (h, m, l)

