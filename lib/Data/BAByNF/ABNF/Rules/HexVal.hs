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
import Data.BAByNF.ABNF qualified as ABNF
import Data.List (uncons)
import Data.BAByNF.Util.List qualified as Util.List
import Data.BAByNF.Util.Hex qualified as Hex


ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "hex-val")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition$ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    $
        [ ABNF.Repetition ABNF.NoRepeat
            . ABNF.CharValElement
            . ABNF.CaseInsensitiveCharVal
            . ABNF.CaseInsensitiveString
            . ABNF.QuotedString
            $ Ascii.stringAsBytesUnsafe "x"
        , ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 1) ABNF.UnBound) (ABNF.RulenameElement Core.hexdigRef)
        , ABNF.Repetition ABNF.NoRepeat
            $ ABNF.OptionElement
            . ABNF.Option
            . ABNF.Alternation
            $
                [ ABNF.Concatenation
                    . List.singleton
                    . ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 1) ABNF.UnBound)
                    . ABNF.GroupElement
                    . ABNF.Group
                    . ABNF.Alternation
                    . List.singleton
                    . ABNF.Concatenation
                    $
                        [ ABNF.Repetition ABNF.NoRepeat . ABNF.CharValElement
                            . ABNF.CaseInsensitiveCharVal
                            . ABNF.CaseInsensitiveString
                            . ABNF.QuotedString
                            $ Ascii.stringAsBytesUnsafe "."
                        , ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 1) ABNF.UnBound) (ABNF.RulenameElement Core.hexdigRef)
                        ]
                , ABNF.Concatenation
                    . List.singleton
                    . ABNF.Repetition ABNF.NoRepeat
                    . ABNF.GroupElement
                    . ABNF.Group
                    . ABNF.Alternation
                    . List.singleton
                    . ABNF.Concatenation
                    $
                        [ ABNF.Repetition ABNF.NoRepeat . ABNF.CharValElement
                            . ABNF.CaseInsensitiveCharVal
                            . ABNF.CaseInsensitiveString
                            . ABNF.QuotedString
                            $ Ascii.stringAsBytesUnsafe "-"
                        , ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 1) ABNF.UnBound) (ABNF.RulenameElement Core.hexdigRef)
                        ]
                ]
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.HexVal
fromTree tree = 
    let nodes = Tree.nodes tree
     in (case uncons nodes of
        Just (h, rest) -> 
            if isB h
                then Right rest
                else Left "hex-val must start with x | X"
        _ -> Left "structural mismatch for <hex-val>")
        >>= takeHexSeq
        >>= \(firstSeq, rest) -> 
            case uncons rest of
                Nothing -> Right (ABNF.SeqHexVal [firstSeq])
                Just (c, rest') | isDash c -> takeHexSeq rest' >>= \(secondSeq, end) -> 
                                        case end of 
                                            [] -> Right (ABNF.RangeHexVal firstSeq secondSeq)
                                            _ -> Left "structural mismatch for <hex-val>"
                                | isDot c -> let takeSeq x = takeHexSeq x >>= (\(nextSeq, rest'') -> case rest'' of
                                                    [] -> Right [nextSeq]
                                                    c':rest''' -> if isDot c' 
                                                                    then takeSeq rest''' >>= \seqs -> Right (nextSeq : seqs)
                                                                    else Left "structural mismatch for <hex-val>")
                                              in takeSeq rest' >>= \seqs -> Right (ABNF.SeqHexVal $ firstSeq : seqs)
                                | otherwise -> Left "structural mismatch for <hex-val>"
    where takeHexSeq :: [Tree.Node ABNF.Rulename] -> Either String (Hex.Seq, [Tree.Node ABNF.Rulename])
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
