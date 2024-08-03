{-# LANGUAGE LambdaCase #-}
module Data.BAByNF.ABNF.Rules.HexVal
    ( ref
    , rule
    , fromTree
    ) where

import Data.List qualified as List

import Data.ByteString qualified as ByteString

import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.List qualified as Util.List
import Data.BAByNF.Util.Hex qualified as Hex
import Data.BAByNF.ABNF.Model qualified as Model


ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "hex-val")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition$ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    $
        [ Model.Repetition Model.NoRepeat
            . Model.CharValElement
            . Model.CaseInsensitiveCharVal
            . Model.CaseInsensitiveString
            . Model.QuotedString
            $ Ascii.stringAsBytesUnsafe "x"
        , Model.Repetition (Model.RangedRepeat (Model.FixedBound 1) Model.UnBound) (Model.RulenameElement Core.hexdigRef)
        , Model.Repetition Model.NoRepeat
            $ Model.OptionElement
            . Model.Option
            . Model.Alternation
            $
                [ Model.Concatenation
                    . List.singleton
                    . Model.Repetition (Model.RangedRepeat (Model.FixedBound 1) Model.UnBound)
                    . Model.GroupElement
                    . Model.Group
                    . Model.Alternation
                    . List.singleton
                    . Model.Concatenation
                    $
                        [ Model.Repetition Model.NoRepeat . Model.CharValElement
                            . Model.CaseInsensitiveCharVal
                            . Model.CaseInsensitiveString
                            . Model.QuotedString
                            $ Ascii.stringAsBytesUnsafe "."
                        , Model.Repetition (Model.RangedRepeat (Model.FixedBound 1) Model.UnBound) (Model.RulenameElement Core.hexdigRef)
                        ]
                , Model.Concatenation
                    . List.singleton
                    . Model.Repetition Model.NoRepeat
                    . Model.GroupElement
                    . Model.Group
                    . Model.Alternation
                    . List.singleton
                    . Model.Concatenation
                    $
                        [ Model.Repetition Model.NoRepeat . Model.CharValElement
                            . Model.CaseInsensitiveCharVal
                            . Model.CaseInsensitiveString
                            . Model.QuotedString
                            $ Ascii.stringAsBytesUnsafe "-"
                        , Model.Repetition (Model.RangedRepeat (Model.FixedBound 1) Model.UnBound) (Model.RulenameElement Core.hexdigRef)
                        ]
                ]
        ]

fromTree :: Tree Model.Rulename -> Either String Model.HexVal
fromTree tree = 
    let nodes = Tree.nodes tree
     in (case List.uncons nodes of
        Just (h, rest) -> 
            if isB h
                then Right rest
                else Left "hex-val must start with x | X"
        _ -> Left "structural mismatch for <hex-val>")
        >>= takeHexSeq
        >>= \(firstSeq, rest) -> 
            case List.uncons rest of
                Nothing -> Right (Model.SeqHexVal [firstSeq])
                Just (c, rest') | isDash c -> takeHexSeq rest' >>= \(secondSeq, end) -> 
                                        case end of 
                                            [] -> Right (Model.RangeHexVal firstSeq secondSeq)
                                            _ -> Left "structural mismatch for <hex-val>"
                                | isDot c -> let takeSeq x = takeHexSeq x >>= (\(nextSeq, rest'') -> case rest'' of
                                                    [] -> Right [nextSeq]
                                                    c':rest''' -> if isDot c' 
                                                                    then takeSeq rest''' >>= \seqs -> Right (nextSeq : seqs)
                                                                    else Left "structural mismatch for <hex-val>")
                                              in takeSeq rest' >>= \seqs -> Right (Model.SeqHexVal $ firstSeq : seqs)
                                | otherwise -> Left "structural mismatch for <hex-val>"
    where takeHexSeq :: [Tree.Node Model.Rulename] -> Either String (Hex.Seq, [Tree.Node Model.Rulename])
          takeHexSeq nodes = case Util.List.lsplitWhenNot nodes isHexDig  of 
                (hexno@(_:_), rest) -> 
                    case Ascii.toHexSeq $ ByteString.concat (map Tree.stringifyNode hexno) of
                        Just hexseq -> Right (hexseq, rest)
                        Nothing -> Left "invalid hex digits in <hex-val>"
                _ -> Left "structural mismatch for <hex-val>"
          isB node = Tree.isStringEq node (Ascii.bs 'x') || Tree.isStringEq node (Ascii.bs 'X')
          isHexDig node = Tree.isRefOf node Core.hexdigRef 
          isDot node = Tree.isStringEq node (Ascii.stringAsBytesUnsafe ".")
          isDash node = Tree.isStringEq node (Ascii.stringAsBytesUnsafe "-")
