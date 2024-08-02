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

import Data.BAByNF.ABNF.Model qualified as Model
import Data.BAByNF.ABNF.Core qualified as Core


ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "bin-val")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition
    $ Model.Elements
    . Model.Alternation
    . List.singleton
    . Model.Concatenation
    $
        [ Model.Repetition Model.NoRepeat
            . Model.CharValElement
            . Model.CaseInsensitiveCharVal
            . Model.CaseInsensitiveString
            . Model.QuotedString
            $ Ascii.stringAsBytesUnsafe "b"
        , Model.Repetition (Model.RangedRepeat (Model.FixedBound 1) Model.UnBound) (Model.RulenameElement Core.bitRef)
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
                        , Model.Repetition (Model.RangedRepeat (Model.FixedBound 1) Model.UnBound) (Model.RulenameElement Core.bitRef)
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
                        , Model.Repetition (Model.RangedRepeat (Model.FixedBound 1) Model.UnBound) (Model.RulenameElement Core.bitRef)
                        ]
                ]
        ]

fromTree :: Tree Model.Rulename -> Either String Model.BinVal
fromTree tree = Stream.runStream_ stream (Tree.nodes tree)
    where stream :: Stream (Tree.Node Model.Rulename) (Either String Model.BinVal)
          stream = expectB `Stream.propagate'`
                   takeByteOrLeft `Stream.propagate`
                   (\firstByte -> Stream.peek >>=
                        (\case
                            Nothing -> return . Right . singleByte $ firstByte
                            Just (Tree.StringNode s)
                                | s == Ascii.bs '-' ->
                                    dashBits <&> (<&> Model.RangeBinVal firstByte)
                                | s == Ascii.bs '.' ->
                                    exhaust dotBits [] <&>
                                    fmap (\byteRest ->
                                            let bytes = firstByte : byteRest
                                             in Model.SeqBinVal bytes
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
          singleByte = Model.SeqBinVal . List.singleton