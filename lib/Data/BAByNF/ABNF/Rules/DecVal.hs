{-# LANGUAGE LambdaCase #-}
module Data.BAByNF.ABNF.Rules.DecVal
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))

import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.Util.Stream (Stream)
import Data.BAByNF.Util.Stream qualified as Stream

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Util.Decimal qualified as Decimal
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "dec-val")

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
            $ Ascii.stringAsBytesUnsafe "d"
        , Model.Repetition (Model.RangedRepeat (Model.FixedBound 1) Model.UnBound) (Model.RulenameElement Core.digitRef)
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
                        , Model.Repetition (Model.RangedRepeat (Model.FixedBound 1) Model.UnBound) (Model.RulenameElement Core.digitRef)
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
                        , Model.Repetition (Model.RangedRepeat (Model.FixedBound 1) Model.UnBound) (Model.RulenameElement Core.digitRef)
                        ]
                ]
        ]

fromTree :: Tree Model.Rulename -> Either String Model.DecVal
fromTree tree = Stream.runStream_ stream (Tree.nodes tree)
    where stream :: Stream (Tree.Node Model.Rulename) (Either String Model.DecVal)
          stream = expectD `Stream.propagate'`
                   takeByteOrLeft `Stream.propagate`
                   (\firstByte -> Stream.peek >>=
                        (\case
                            Nothing -> return . Right . singleByte $ firstByte
                            Just (Tree.StringNode s)
                                | s == Ascii.bs '-' ->
                                    dashDecimals <&> (<&> Model.RangeDecVal firstByte)
                                | s == Ascii.bs '.' ->
                                    exhaust dotDecimals [] <&>
                                    fmap (\byteRest ->
                                            let bytes = firstByte : byteRest
                                             in Model.SeqDecVal bytes
                                         )
                                | otherwise -> return (Left "unexpected char")
                            _ -> return (Left "dec-num pattern not matched")
                        )
                   )
          expectD = Stream.takeIf isD <&> maybe (Left "expected d or D") (const (Right ()))
          isD node = Tree.isStringEq node (Ascii.bs 'd') || Tree.isStringEq node (Ascii.bs 'D')
          nodeToDecimal node =  if not (Tree.isRefOf node Core.digitRef) then Nothing
            else let b = Tree.stringifyNode node in Ascii.bsToDecimalDigit b
          takeByte = Stream.takeWhileMap nodeToDecimal <&>
            \case
                [] -> Nothing
                decimals -> Just (Decimal.Seq decimals)
          takeByteOrLeft = takeByte <&> maybe (Left "not digits") Right
          dashDecimals = (Stream.takeIf (`Tree.isStringEq` Ascii.bs '-') `Stream.propagate'` takeByte) <&> maybe (Left "not dash-bits") Right
          dotDecimals = (Stream.takeIf (`Tree.isStringEq` Ascii.bs '.') `Stream.propagate'` takeByte) <&> maybe (Left "not dot-bits") Right
          exhaust m acc = Stream.hasNext >>= \cond -> if cond
                                then m `Stream.propagate` (\e -> exhaust m (e:acc))
                                else return (Right (reverse acc))
          singleByte = Model.SeqDecVal . List.singleton