{-# LANGUAGE LambdaCase #-}
module Data.BAByNF.ABNF.Rules.BinVal
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))
import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.Binary qualified as Binary
import Data.BAByNF.Util.Stream (Stream)
import Data.BAByNF.Util.Stream qualified as Stream

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Core qualified as Core


ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "bin-val")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition
    $ ABNF.Elements
    . ABNF.Alternation
    . List.singleton
    . ABNF.Concatenation
    $
        [ ABNF.Repetition ABNF.NoRepeat
            . ABNF.CharValElement
            . ABNF.CaseInsensitiveCharVal
            . ABNF.CaseInsensitiveString
            . ABNF.QuotedString
            $ Ascii.stringAsBytesUnsafe "b"
        , ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 1) ABNF.UnBound) (ABNF.RulenameElement Core.bitRef)
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
                        , ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 1) ABNF.UnBound) (ABNF.RulenameElement Core.bitRef)
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
                        , ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 1) ABNF.UnBound) (ABNF.RulenameElement Core.bitRef)
                        ]
                ]
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.BinVal
fromTree tree = Stream.runStream_ stream (Tree.nodes tree)
    where stream :: Stream (Tree.Node ABNF.Rulename) (Either String ABNF.BinVal)
          stream = expectB `Stream.propagate'`
                   takeByteOrLeft `Stream.propagate`
                   (\firstByte -> Stream.peek >>=
                        (\case
                            Nothing -> return . Right . singleByte $ firstByte
                            Just (Tree.StringNode s)
                                | s == Ascii.bs '-' ->
                                    dashBits <&> (<&> ABNF.RangeBinVal firstByte)
                                | s == Ascii.bs '.' ->
                                    exhaust dotBits [] <&>
                                    fmap (\byteRest ->
                                            let bytes = firstByte : byteRest
                                             in ABNF.SeqBinVal bytes
                                         )
                                | otherwise -> return (Left "unexpected char")
                            _ -> return (Left "bin-num pattern not matched")
                        )
                   )
          expectB = Stream.takeIf isB <&> maybe (Left "expected b or B") (const (Right ()))
          isB node = Tree.isStringEq node (Ascii.bs 'b') || Tree.isStringEq node (Ascii.bs 'B')
          nodeToBit node =  if not (Tree.isRefOf node Core.bitRef) then Nothing
            else let b = Tree.stringifyNode node in
                if b == Ascii.bs '0' then Just Binary.B0 
                else if b == Ascii.bs '1' then Just Binary.B1 
                else Nothing
          takeByte = Stream.takeWhileMap nodeToBit <&>
            \case
                [] -> Nothing
                bits -> Just (Binary.Seq bits)
          takeByteOrLeft = takeByte <&> maybe (Left "not bits") Right
          dashBits = (Stream.takeIf (`Tree.isStringEq` Ascii.bs '-') `Stream.propagate'` takeByte) <&> maybe (Left "not dash-bits") Right
          dotBits = (Stream.takeIf (`Tree.isStringEq` Ascii.bs '.') `Stream.propagate'` takeByte) <&> maybe (Left "not dot-bits") Right
          exhaust m acc = Stream.hasNext >>= \cond -> if cond
                                then m `Stream.propagate` (\e -> exhaust m (e:acc))
                                else return (Right (reverse acc))
          singleByte = ABNF.SeqBinVal . List.singleton