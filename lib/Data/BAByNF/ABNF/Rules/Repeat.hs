module Data.BAByNF.ABNF.Rules.Repeat
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))
import Data.Maybe qualified as Maybe
import Data.List qualified as List

import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.Util.Stream qualified as Stream
import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Core.Ref qualified as Ref



ref :: ABNF.Rulename
ref = ABNF.Rulename (Ascii.stringAsBytesUnsafe "repeat")

rule :: ABNF.Rule
rule = ABNF.Rule ref ABNF.BasicDefinition
    . ABNF.Elements
    . ABNF.Alternation
    $ 
        [ ABNF.Concatenation
            . List.singleton
            . ABNF.Repetition (ABNF.RangedRepeat (ABNF.FixedBound 1) ABNF.UnBound)
            $ ABNF.RulenameElement Core.digitRef 
        , ABNF.Concatenation
            . List.singleton
            . ABNF.Repetition ABNF.NoRepeat
            . ABNF.GroupElement
            . ABNF.Group
            . ABNF.Alternation
            . List.singleton
            . ABNF.Concatenation
            $  
                [ ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound)
                    $ ABNF.RulenameElement Core.digitRef
                , ABNF.Repetition ABNF.NoRepeat
                    . ABNF.CharValElement 
                    . ABNF.CaseInsensitiveCharVal
                    . ABNF.CaseInsensitiveString
                    . ABNF.QuotedString
                    $ Ascii.stringAsBytesUnsafe "*"
                , ABNF.Repetition (ABNF.RangedRepeat ABNF.UnBound ABNF.UnBound)
                    $ ABNF.RulenameElement Core.digitRef 
                ]
        ]

fromTree :: Tree ABNF.Rulename -> Either String ABNF.Repeat
fromTree tree =
    let stream = do
            mnOpt <- takeDigits
            hasStar <- Stream.take <&> Maybe.isJust
            mxOpt <- if hasStar then takeDigits else return Nothing
            case (mnOpt, hasStar, mxOpt) of
                (Just mns, False, _) -> return $ tryToInteger mns >>= \mn -> return $ ABNF.FixedRepeat mn
                (_, True, _) -> return $
                    let toBound = Maybe.maybe (Right ABNF.UnBound) (\x -> tryToInteger x >>= return . ABNF.FixedBound)
                     in do 
                        lo <- toBound mnOpt
                        hi <- toBound mxOpt
                        Right (ABNF.RangedRepeat lo hi)
                _ -> return $ Left "structural mismatch for <repeat>"
     in Stream.runStream_ stream (Tree.nodes tree)
    where takeDigits = Stream.takeWhileMap (\e ->
            case e of
                Tree.RefNode r subtree ->
                    if Ref.eq r Core.digitRef
                        then Just $ Tree.stringify subtree
                        else Nothing
                _ -> Nothing
            ) <&> \bs -> case bs of [] -> Nothing; _ -> Just . ByteString.concat $ bs
          tryToInteger bs =
              case ByteString.Char8.readInteger bs of
                  Nothing -> Left "not integer"
                  Just (no, rest) | ByteString.null rest  -> Right no
                                  | otherwise -> Left "more than an integer read"
