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

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Core qualified as Core

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "quoted-string")

rule :: ABNF.Rule
rule = ABNF.Rule ref  ABNF.BasicDefinition
    ( ABNF.Elements $ 
        ABNF.Alternation [ABNF.Concatenation 
            [ ABNF.Repetition ABNF.NoRepeat (ABNF.RulenameElement Core.dquoteRef)
            , ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound) (ABNF.GroupElement . ABNF.Group . ABNF.Alternation $
                    [ ABNF.Concatenation [ ABNF.Repetition ABNF.NoRepeat $ ABNF.NumValElement . ABNF.HexNumVal $ 
                        ABNF.RangeHexVal (Hex.Seq [Hex.X2, Hex.X0]) (Hex.Seq [Hex.X2, Hex.X1])]
                    , ABNF.Concatenation [ ABNF.Repetition ABNF.NoRepeat $ ABNF.NumValElement . ABNF.HexNumVal $
                        ABNF.RangeHexVal (Hex.Seq [Hex.X2, Hex.X3]) (Hex.Seq [Hex.X7, Hex.XE])]
                    ])
                
            , ABNF.Repetition ABNF.NoRepeat (ABNF.RulenameElement Core.dquoteRef)
            ]
        ]
    )

fromTree :: Tree ABNF.Rulename -> Either String ABNF.QuotedString -- TODO: support RFC 7405
fromTree tree =
    maybe (Left "quoted-string must be between \" and \"") Right $
        unconsnoc (Tree.stringify tree) >>= \(h, m, l) ->
            if h == 34 && l == 34
                then Just (ABNF.QuotedString m)
                else Nothing

unconsnoc :: ByteString -> Maybe (Word8, ByteString, Word8)
unconsnoc bs = ByteString.uncons bs >>= \(h, t) -> ByteString.unsnoc t <&> \(m, l) -> (h, m, l)

