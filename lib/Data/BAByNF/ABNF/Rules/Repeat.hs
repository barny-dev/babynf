module Data.BAByNF.ABNF.Rules.Repeat
    ( ref
    , rule
    , fromTree
    ) where

import Data.Functor ((<&>))
import Data.Maybe qualified as Maybe

import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8

import Data.BAByNF.Core.Tree (Tree)
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.ABNF.Grammar ((+!), (|!))
import Data.BAByNF.ABNF.Grammar qualified as Grammar
import Data.BAByNF.ABNF.Core qualified as Core
import Data.BAByNF.Util.Stream qualified as Stream



ref :: Grammar.RuleRef
ref = Grammar.ref "repeat"

rule :: Grammar.RuleDecl
rule = Grammar.RuleDecl ref 
    ( Grammar.RuleDef $
        ( Grammar.oneOrMore . Grammar.asElem $ Core.digitRef) |!
        ( Grammar.group . Grammar.asAlt $  
            (Grammar.zeroOrMore . Grammar.asElem $ Core.digitRef) +!
            (Grammar.str "*") +!
            (Grammar.zeroOrMore . Grammar.asElem $ Core.digitRef)))

fromTree :: Tree Grammar.ABNFRef -> Either String Grammar.RepDef
fromTree tree =
    let stream = do
            mnOpt <- takeDigits
            hasStar <- Stream.take <&> Maybe.isJust
            mxOpt <- if hasStar then takeDigits else return Nothing
            case (mnOpt, hasStar, mxOpt) of
                (Just mns, _, Nothing) -> return $ tryToInteger mns >>= \mn -> return $ Grammar.RepDef (Just mn) (if hasStar then Nothing else Just mn)
                (Just mns, True, Just mxs) -> return $ tryToInteger mns >>= \mn -> tryToInteger mxs >>= \mx -> return $ Grammar.RepDef (Just mn) (Just mx)
                (Nothing, True, Just mxs) -> return $ tryToInteger mxs >>= \mx -> return $ Grammar.RepDef Nothing (Just mx)
                _ -> return $ Left "illegal state"
     in Stream.runStream_ stream (Tree.nodes tree)
    where takeDigits = Stream.takeWhileMap (\e ->
            case e of
                Tree.RefNode r subtree ->
                    if r == (Grammar.toRef Core.digitRef)
                        then Just $ Tree.stringify subtree
                        else Nothing
                _ -> Nothing
            ) <&> \bs -> case bs of [] -> Nothing; _ -> Just . ByteString.concat $ bs
          tryToInteger bs =
              case ByteString.Char8.readInteger bs of
                  Nothing -> Left "not integer"
                  Just (no, rest) | ByteString.null rest  -> Left "more than an integer read"
                                  | otherwise -> Right no
