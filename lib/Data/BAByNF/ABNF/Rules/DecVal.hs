{-# LANGUAGE LambdaCase #-}
module Data.BAByNF.ABNF.Rules.DecVal
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString

import Data.BAByNF.ABNF.Grammar ((+!), (|!))
import Data.BAByNF.ABNF.Grammar qualified as Grammar
import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.Util.Stream (Stream)
import Data.BAByNF.Util.Stream qualified as Stream

import Data.List qualified as List

import Data.BAByNF.Util.Ascii qualified as Ascii

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.Util.Decimal qualified as Decimal

ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "dec-val")

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
            $ Ascii.stringAsBytesUnsafe "d"
        , ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 1) ABNF.UnBound) (ABNF.RulenameElement Core.digitRef)
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
                        , ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 1) ABNF.UnBound) (ABNF.RulenameElement Core.digitRef)
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
                        , ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 1) ABNF.UnBound) (ABNF.RulenameElement Core.digitRef)
                        ]
                ]
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.DecVal
fromTree tree = Stream.runStream_ stream (Tree.nodes tree)
    where stream :: Stream (Tree.Node ABNF.Rulename) (Either String ABNF.DecVal)
          stream = expectD `Stream.propagate'`
                   takeByteOrLeft `Stream.propagate`
                   (\firstByte -> Stream.peek >>=
                        (\case
                            Nothing -> return . Right . singleByte $ firstByte
                            Just (Tree.StringNode s)
                                | s == Ascii.bs '-' ->
                                    dashDecimals <&> (<&> ABNF.RangeDecVal firstByte)
                                | s == Ascii.bs '.' ->
                                    exhaust dotDecimals [] <&>
                                    fmap (\byteRest ->
                                            let bytes = firstByte : byteRest
                                             in ABNF.SeqDecVal bytes
                                         )
                                | otherwise -> return (Left "unexpected char")
                            _ -> return (Left "bin-num pattern not matched")
                        )
                   )
          expectD = Stream.takeIf isD <&> maybe (Left "expected d or D") (const (Right ()))
          isD node = Tree.isStringEq node (Ascii.bs 'd') || Tree.isStringEq node (Ascii.bs 'D')
          nodeToDecimal node =  if not (Tree.isRefOf node Core.bitRef) then Nothing
            else let b = Tree.stringifyNode node in Ascii.bsToDecimalDigit b
          takeByte = Stream.takeWhileMap nodeToDecimal <&>
            \case
                [] -> Nothing
                decimals -> Just (Decimal.Seq decimals)
          takeByteOrLeft = takeByte <&> maybe (Left "not bits") Right
          dashDecimals = (Stream.takeIf (`Tree.isStringEq` Ascii.bs '-') `Stream.propagate'` takeByte) <&> maybe (Left "not dash-bits") Right
          dotDecimals = (Stream.takeIf (`Tree.isStringEq` Ascii.bs '.') `Stream.propagate'` takeByte) <&> maybe (Left "not dot-bits") Right
          exhaust m acc = Stream.hasNext >>= \cond -> if cond
                                then m `Stream.propagate` (\e -> exhaust m (e:acc))
                                else return (Right (reverse acc))
          singleByte = ABNF.SeqDecVal . List.singleton


    -- Stream.runStream_ stream (Tree.nodes tree)
    -- where stream = takeD `Stream.propagate'`
    --                takeNum `Stream.propagate`
    --                 \num -> Stream.peek >>= \case
    --                      Nothing -> return . Right $ Grammar.ArrayTerm Grammar.CaseSensitive (ByteString.singleton num)
    --                      Just node | node `Tree.isStringEq` (Ascii.bs '-') -> dashNum `Stream.propagate` \num2 -> return . Right $ Grammar.RangeTerm num num2
    --                                | node `Tree.isStringEq` (Ascii.bs '.') -> exhaust dotNum [num] `Stream.propagate` \nums -> return . Right $ Grammar.ArrayTerm Grammar.CaseSensitive (ByteString.pack nums)
    --                                | otherwise -> return (Left "unexpected char")
    --       takeD = Stream.takeIf (\node -> node `Tree.isStringEq` Ascii.bs 'd' || node `Tree.isStringEq` Ascii.bs 'D') <&> maybe (Left "expected d or D") (const (Right ()))
    --       takeNum = Stream.takeWhile (\n -> n `Tree.isRefOf` Grammar.toRef Core.digitRef) <&> \case [] -> Left "no decimal num"; nodes -> toDecNum . ByteString.concat $ map Tree.stringifyNode nodes
    --       toDecNum bs = toDecNum' bs 0
    --       toDecNum' bs acc =
    --           case ByteString.uncons bs of
    --               Nothing -> Right acc
    --               Just (h, r) | h >= 48 && h <= 57 -> toDecNum' r ((acc * 10) + (h - 48))
    --                           | otherwise -> Left "non-decimal digit"
    --       dashNum = (Stream.takeIf (`Tree.isStringEq` Ascii.bs '-') <&> maybe (Left "dash expected") (const (Right ()))) `Stream.propagate'` takeNum
    --       dotNum = (Stream.takeIf (`Tree.isStringEq` Ascii.bs '.') <&> maybe (Left "dash expected") (const (Right ()))) `Stream.propagate'` takeNum
    --       exhaust m acc = Stream.hasNext >>= \cond -> if cond
    --                             then m `Stream.propagate` (\e -> exhaust m (e:acc))
    --                             else return (Right (reverse acc))


