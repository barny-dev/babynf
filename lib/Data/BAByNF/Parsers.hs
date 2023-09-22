{-# LANGUAGE FunctionalDependencies #-}
module Data.BAByNF.Parsers where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (toList)

import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as AP

import qualified Data.BAByNF.Grammar as G
import qualified Data.BAByNF.ParseTree as PT
import qualified Data.BAByNF.Util.Ascii as A

ruleParser :: G.Grammar -> G.RuleRef -> AP.Parser PT.RuleNode
ruleParser grammar ref =
    case G.getMergedDecl grammar ref of
        Nothing -> error $ "non-existing rule " ++ show ref  -- TODO: proper error handling
        Just def@(G.RuleDecl _ (G.RuleDef alt)) -> do
            (match, alt) <- AP.match $ altParser grammar alt
            return PT.RuleNode
                { PT.ruleRef = ref
                , PT.ruleMatch = match
                , PT.ruleInner = alt
                , PT.ruleDef = def}

altParser :: G.Grammar -> G.Alt -> AP.Parser PT.AltNode
altParser grammar def@(G.Alt nonEmpty) =
    let attempt i x = do
            (match, concat) <- AP.match $ concatParser grammar x
            return PT.AltNode
                { PT.altMatch = match
                , PT.altSelect = concat
                , PT.altSelectNo = i
                , PT.altDef = def
                }
        nextAttempt i alts =
            case alts of
                 [] -> fail "no more alternatives"
                 (x:xs) -> (attempt i x) <|> (nextAttempt (i + 1) xs)
    in nextAttempt 0 (toList nonEmpty)

concatParser :: G.Grammar -> G.Concat -> AP.Parser PT.ConcatNode
concatParser grammar def@(G.Concat nonEmpty) =
    let parseRep rep = repParser grammar rep
    in do
        (match, repVals) <- AP.match $ mapM parseRep nonEmpty
        return PT.ConcatNode
            { PT.concatMatch = match
            , PT.concatVals = repVals
            , PT.concatDef = def
            }

repParser :: G.Grammar -> G.Rep -> AP.Parser PT.RepNode
repParser grammar def@(G.Rep maybeRepDef e) = do 
    (match, instances) <- AP.match (proceed 0)
    return PT.RepNode
        { PT.repMatch = match
        , PT.repInstances = instances
        , PT.repDef = def
        }
    where proceed count = case repState maybeRepDef count of
            Invalid -> error $ "invalid RepDef" ++ show maybeRepDef
            Satisfied -> return []
            NeedMore -> do
                x <- elemParser grammar e
                xs <- proceed (count + 1)
                return (x:xs)
            WantMore -> (do
                x <- elemParser grammar e
                xs <- proceed (count + 1)
                return (x:xs)) <|> (return []) 

data RepState = NeedMore | Satisfied | WantMore | Invalid

repState :: Maybe G.RepDef -> Integer -> RepState
repState Nothing count = if count < 1 then NeedMore else Satisfied
repState (Just (G.RepDef maybeMin maybeMax)) count = 
    if invalid maybeMin maybeMax
        then Invalid
        else let minReached = count >= (fromMaybe 0 maybeMin)
                 maxReached = maybe False (\mx -> count >= mx) maybeMax
             in case (minReached, maxReached) of
                (False, True) -> Invalid
                (True, True) -> Satisfied
                (True, False) -> WantMore
                (False, False) -> NeedMore
    where invalid a b =
            case (a,b) of
                (Nothing, Nothing) -> False
                (Just mn, Just mx) -> mn < 0 || mn > mx
                (Just mn, _) -> mn < 0
                (_, Just mx) -> mx < 1


elemParser :: G.Grammar -> G.Elem -> AP.Parser PT.ElemNode
elemParser grammar def =
    let innerParser = case def of 
            G.RuleRefE ref -> (ruleParser grammar ref) >>= (return . PT.RuleNodeVal)
            G.GroupE group -> (groupParser grammar group) >>= (return . PT.GroupNodeVal)
            G.OptE opt -> (optParser grammar opt) >>= (return . PT.OptNodeVal)
            G.TermE term -> (termParser term) >>= (return . PT.TermNodeVal)
    in do
        (match, val) <- AP.match innerParser
        return PT.ElemNode
            { PT.elemMatch = match
            , PT.elemVal = val
            , PT.elemDef = def
            }

groupParser :: G.Grammar -> G.Group -> AP.Parser PT.GroupNode
groupParser grammar def@(G.Group alt) = do
    (match, alt) <- AP.match (altParser grammar alt)
    return PT.GroupNode
        { PT.groupMatch = match
        , PT.groupInner = alt
        , PT.groupDef = def
        }

optParser :: G.Grammar -> G.Opt -> AP.Parser PT.OptNode
optParser grammar def@(G.Opt alt) =
    let innerParser = ((altParser grammar alt) >>= (return . Just)) <|> (return Nothing) 
    in do
        (match, maybeAltp) <- AP.match innerParser
        return PT.OptNode
            { PT.optMatch = match
            , PT.optInner = maybeAltp
            , PT.optDef = def
            }

termParser :: G.Term -> AP.Parser PT.TermNode
termParser def = do
    match <- case def of
        (G.RangeTerm lo hi) -> AP.satisfy (\x -> x >= lo && x <= hi) >>= return . BS.singleton
        (G.ArrayTerm G.CaseSensitive bs) -> AP.string bs
        (G.ArrayTerm G.CaseInsensitive bs) ->
            AP.take (BS.length bs) >>=
            (\match -> if A.eqNoCaseBS match bs
                then return match
                else fail "Term not matched")
    return PT.TermNode
        { PT.termMatch = match
        , PT.termDef = def
        }
