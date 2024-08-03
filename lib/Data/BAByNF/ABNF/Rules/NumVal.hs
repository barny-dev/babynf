module Data.BAByNF.ABNF.Rules.NumVal
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))
import Data.List qualified as List

import Data.ByteString qualified as ByteString

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Rules.BinVal qualified as BinVal
import Data.BAByNF.ABNF.Rules.DecVal qualified as DecVal
import Data.BAByNF.ABNF.Rules.HexVal qualified as HexVal
import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Core.Ref qualified as Ref
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "num-val")

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
            $ Model.QuotedString (Ascii.stringAsBytesUnsafe "%")
            
        , Model.Repetition Model.NoRepeat
            . Model.GroupElement
            . Model.Group
            . Model.Alternation
            $ 
                [ Model.Concatenation
                    . List.singleton
                    . Model.Repetition Model.NoRepeat
                    $ Model.RulenameElement BinVal.ref
                , Model.Concatenation
                    . List.singleton
                    . Model.Repetition Model.NoRepeat
                    $ Model.RulenameElement DecVal.ref
                , Model.Concatenation
                    . List.singleton
                    . Model.Repetition Model.NoRepeat
                    $ Model.RulenameElement HexVal.ref
                ]
        ]

fromTree :: Tree Model.Rulename -> Either String Model.NumVal
fromTree tree = case Tree.nodes tree of
    [Tree.StringNode b, Tree.RefNode r subtree] ->
        if b /= ByteString.singleton 37
            then Left "expected % prefix"
            else if Ref.eq r BinVal.ref then BinVal.fromTree subtree <&> Model.BinNumVal
                else if Ref.eq r DecVal.ref then DecVal.fromTree subtree <&> Model.DecNumVal
                else if Ref.eq r HexVal.ref then HexVal.fromTree subtree <&> Model.HexNumVal
                else Left "unexpected rule ref"
    _ -> Left "exactly two nodes expected - \"%\" and bin-val | dec-val | hex-val"

