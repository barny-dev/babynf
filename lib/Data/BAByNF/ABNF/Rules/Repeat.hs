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
import Data.BAByNF.Util.Ascii qualified as Ascii
import Data.BAByNF.Core.Ref qualified as Ref
import Data.BAByNF.ABNF.Model qualified as Model

ref :: Model.Rulename
ref = Model.Rulename (Ascii.stringAsBytesUnsafe "repeat")

rule :: Model.Rule
rule = Model.Rule ref Model.BasicDefinition
    . Model.Elements
    . Model.Alternation
    $ 
        [ Model.Concatenation
            . List.singleton
            . Model.Repetition (Model.RangedRepeat (Model.FixedBound 1) Model.UnBound)
            $ Model.RulenameElement Core.digitRef 
        , Model.Concatenation
            . List.singleton
            . Model.Repetition Model.NoRepeat
            . Model.GroupElement
            . Model.Group
            . Model.Alternation
            . List.singleton
            . Model.Concatenation
            $  
                [ Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound)
                    $ Model.RulenameElement Core.digitRef
                , Model.Repetition Model.NoRepeat
                    . Model.CharValElement 
                    . Model.CaseInsensitiveCharVal
                    . Model.CaseInsensitiveString
                    . Model.QuotedString
                    $ Ascii.stringAsBytesUnsafe "*"
                , Model.Repetition (Model.RangedRepeat Model.UnBound Model.UnBound)
                    $ Model.RulenameElement Core.digitRef 
                ]
        ]

fromTree :: Tree Model.Rulename -> Either String Model.Repeat
fromTree tree =
    let stream = do
            mnOpt <- takeDigits
            hasStar <- Stream.take <&> Maybe.isJust
            mxOpt <- if hasStar then takeDigits else return Nothing
            case (mnOpt, hasStar, mxOpt) of
                (Just mns, False, _) -> return $ tryToInteger mns >>= \mn -> return $ Model.FixedRepeat mn
                (_, True, _) -> return $
                    let toBound = Maybe.maybe (Right Model.UnBound) (\x -> tryToInteger x >>= return . Model.FixedBound)
                     in do 
                        lo <- toBound mnOpt
                        hi <- toBound mxOpt
                        Right (Model.RangedRepeat lo hi)
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
