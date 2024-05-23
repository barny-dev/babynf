module Data.BAByNF.Core.Parseable
    ( Parseable (..)
    , toParser
    , Dict
    , TreeParser
    ) where

import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty

import Data.Attoparsec.ByteString qualified as Attoparsec

import Data.BAByNF.Core.Tree (Tree, Node (..))
import Data.BAByNF.Core.Tree qualified as Tree
import Data.BAByNF.Core.Ref (Ref)
import Data.BAByNF.Core.Ref qualified as Ref
import Data.BAByNF.Core.RefDict (RefDict)
import Data.BAByNF.Core.RefDict qualified as RefDict
import Data.BAByNF.Core.Repeat (Repeat, RepeatCount)
import Data.BAByNF.Core.Repeat qualified as Repeat

type Dict a = RefDict a (Parseable a)
type TreeParser a = Attoparsec.Parser (Tree a)

data ParserEnvironment a where
    ParserEnvironment :: (Ref a) =>  { parserGrammar :: Dict a, parserContextStack :: [ParserContext a] } -> ParserEnvironment a
deriving instance (Show a) => Show (ParserEnvironment a)

data ParserState a where
    ParserState :: (Ref a) => { parserEnvironment ::  ParserEnvironment a , parserFocus :: ParserFocus a } -> ParserState a
deriving instance (Show a) => Show (ParserState a)

data ParserContext a where
    SeqContext :: (Ref a) => { seqPrev :: Tree a, seqNext :: [Parseable a]} -> ParserContext a
    RepContext :: (Ref a) => { repPrev :: Tree a, repParse :: Parseable a, repCount :: RepeatCount} -> ParserContext a
    AltContext :: (Ref a) => { altNext ::  [Parseable a]} -> ParserContext a
    RuleContext :: (Ref a) => { ruleRef :: a } -> ParserContext a
deriving instance (Show a) => Show (ParserContext a)

data Parseable a where
    Seq :: (Ref a) => NonEmpty (Parseable a) -> Parseable a
    Alt :: (Ref a) => NonEmpty (Parseable a) -> Parseable a
    Rep :: (Ref a) => (Parseable a) -> Repeat -> Parseable a
    Rule :: (Ref a) => a -> Parseable a
    Unit :: String -> (TreeParser a) -> Parseable a

instance (Show a) => Show (Parseable a) where
    show :: Parseable a -> String
    show x = case x of
        Seq y -> "Seq " ++ show y
        Alt y -> "Alt " ++ show y
        Rep a b -> "Rep (" ++ show b ++ ", " ++ show b ++ ") " ++ show a
        Rule r -> "Rule " ++ show r
        Unit desc _ -> "Unit <" ++ desc ++ ">"

data ParserFocus a where
    Before :: Ref a => Parseable a -> ParserFocus a
    OnReturn :: Ref a => ParserContext a -> Tree a -> ParserFocus a
    OnFailure :: Ref a => ParserContext a -> ParserFocus a
    After :: Ref a => Tree a -> ParserFocus a
deriving instance (Show a) => Show (ParserFocus a)

toParser :: (Ref a) => Dict a -> Parseable a -> TreeParser a
toParser grammar parseable = toParser' $ ParserState { parserEnvironment = ParserEnvironment { parserGrammar = grammar, parserContextStack = []}, parserFocus = Before parseable }

alts :: (Ref a) => [Parseable a] -> Maybe (Parseable a)
alts [] = Nothing
alts [x] = Just x
alts (x:xs) = Just $ Alt (x :| xs)

data Action a where
    Return :: Ref a => Tree a -> Action a
    Split :: Ref a => ParserContext a -> Parseable a -> Action a
    Branch :: Ref a => ParserContext a -> Parseable a -> Action a
    Parse :: Ref a => TreeParser a -> Action a
    Panic :: String -> Action a

applyAction :: (Ref a) => ParserEnvironment a -> Action a -> TreeParser a
applyAction env (Return tree) =
    case pop env of
        Nothing -> return tree
        Just (env', ctx) -> toParser' ParserState { parserEnvironment = env', parserFocus = OnReturn ctx tree }
applyAction env (Split ctx p) = let env' = push env ctx in toParser' ParserState { parserEnvironment = env', parserFocus = Before p  }
applyAction env (Branch ctx p) = Attoparsec.choice [ applyAction env (Split ctx p), toParser' ParserState { parserEnvironment = env, parserFocus = OnFailure ctx }]
applyAction env (Parse p) = p >>= \tree -> toParser' $ ParserState { parserEnvironment = env,  parserFocus = After tree }
applyAction _ (Panic withMsg) = fail withMsg

pop :: (Ref a) => ParserEnvironment a -> Maybe (ParserEnvironment a, ParserContext a)
pop ParserEnvironment { parserGrammar = grammar, parserContextStack = contextStack } =
    case contextStack of
        [] -> Nothing
        ctx : rest -> Just (ParserEnvironment { parserGrammar = grammar, parserContextStack = rest }, ctx)

push :: (Ref a) => ParserEnvironment a -> ParserContext a -> ParserEnvironment a
push ParserEnvironment { parserGrammar = grammar, parserContextStack = contextStack} ctx = ParserEnvironment { parserGrammar = grammar, parserContextStack = ctx : contextStack }

toParser' :: Ref a => ParserState a -> TreeParser a
toParser' state =
    let action = case parserFocus state of
            Before (Unit _ p) -> Parse p
            Before (Rule ref) ->
                let maybeP = lookupDef ref (parserEnvironment state)
                 in maybe (Panic $ "undefined " ++ Ref.display ref) (Split RuleContext { ruleRef = ref }) maybeP
            Before (Seq (p :| ps)) -> Split SeqContext { seqPrev = Tree.empty, seqNext = ps } p
            Before (Alt (p :| ps)) -> Branch AltContext { altNext = ps} p
            Before (Rep p rep) ->
                let rc = Repeat.initCount rep
                 in case Repeat.state rc of
                    Repeat.Satisfied -> Return Tree.empty
                    Repeat.WantMore -> Branch RepContext { repPrev = Tree.empty, repParse = p, repCount = rc } p
                    Repeat.NeedMore -> Split RepContext { repPrev = Tree.empty, repParse = p, repCount = rc} p
            OnReturn SeqContext { seqPrev = prev, seqNext = next } tree ->
                case next of
                    [] -> Return $ prev <> tree
                    p : next' -> Split SeqContext { seqPrev = prev <> tree, seqNext = next' } p
            OnReturn AltContext { altNext = _ } tree -> Return tree
            OnReturn RepContext { repParse = p, repPrev = prev, repCount = rc } tree ->
                case Repeat.tryIncrementCount rc of
                    Nothing -> Panic "repetitions already satisfied"
                    Just rc' ->
                        case Repeat.state rc' of
                            Repeat.Satisfied -> Return $ prev <> tree
                            Repeat.WantMore -> Branch RepContext { repPrev = prev <> tree, repParse = p, repCount = rc' } p
                            Repeat.NeedMore -> Split RepContext { repPrev = prev <> tree, repParse = p, repCount = rc' } p
            OnReturn RuleContext { ruleRef = ref } tree -> Return $ Tree.singleton $ Tree.RefNode ref tree
            OnFailure AltContext { altNext = next } ->
                case next of
                    [] -> Panic "no more alts"
                    p : next' -> Branch AltContext { altNext = next' } p
            OnFailure RepContext { repParse = _, repPrev = prev, repCount = rc } ->
                case Repeat.state rc of
                    Repeat.NeedMore -> Panic "more repetitions required"
                    _ -> Return prev
            OnFailure _ -> Panic "failure in non-safeguarded context"
            After tree -> Return tree
     in applyAction (parserEnvironment state) action

lookupDef :: Ref a => a -> ParserEnvironment a -> Maybe (Parseable a)
lookupDef ref env = lookupDef' ref (parserGrammar env)
lookupDef' :: Ref a => a -> Dict a -> Maybe (Parseable a)
lookupDef' ref grammar = alts $ RefDict.lookup ref grammar
