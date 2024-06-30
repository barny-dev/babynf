module Data.BAByNF.ABNF.Rules.NumVal
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))
import Data.ByteString qualified as ByteString
import Data.List qualified as List
import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Rules.BinVal qualified as BinVal
import Data.BAByNF.ABNF.Rules.DecVal qualified as DecVal
import Data.BAByNF.ABNF.Rules.HexVal qualified as HexVal
import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.Core.Ref qualified as Ref

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "num-val")

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
            $ ABNF.QuotedString (Ascii.stringAsBytesUnsafe "%")
            
        , ABNF.Repetition ABNF.NoRepeat
            . ABNF.GroupElement
            . ABNF.Group
            . ABNF.Alternation
            $ 
                [ ABNF.Concatenation
                    . List.singleton
                    . ABNF.Repetition ABNF.NoRepeat
                    $ ABNF.RulenameElement BinVal.ref
                , ABNF.Concatenation
                    . List.singleton
                    . ABNF.Repetition ABNF.NoRepeat
                    $ ABNF.RulenameElement DecVal.ref
                , ABNF.Concatenation
                    . List.singleton
                    . ABNF.Repetition ABNF.NoRepeat
                    $ ABNF.RulenameElement HexVal.ref
                ]
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.NumVal
fromTree tree = case Tree.nodes tree of
    [Tree.StringNode b, Tree.RefNode r subtree] ->
        if b /= ByteString.singleton 37
            then Left "expected % prefix"
            else if Ref.eq r BinVal.ref then BinVal.fromTree subtree <&> ABNF.BinNumVal
                else if Ref.eq r DecVal.ref then DecVal.fromTree subtree <&> ABNF.DecNumVal
                else if Ref.eq r HexVal.ref then HexVal.fromTree subtree <&> ABNF.HexNumVal
                else Left "unexpected rule ref"
    _ -> Left "exactly two nodes expected - \"%\" and bin-val | dec-val | hex-val"

