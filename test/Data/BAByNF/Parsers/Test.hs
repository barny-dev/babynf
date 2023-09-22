module Data.BAByNF.Parsers.Test (tests) where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as TH

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as B8 ()

import qualified Data.BAByNF.Grammar as BG (RuleDecl (..), ref)
import qualified Data.BAByNF.Grammar.Core as Core
import qualified Data.BAByNF.Grammar.ABNF as ABNF
import qualified Data.BAByNF.Parsers as BP (ruleParser)
import qualified Data.BAByNF.ParseTree as BT
import qualified Data.BAByNF.SimpleTree as BS
import Data.String (IsString(fromString) ) 

import qualified Test.Tasty.Patterns.Parser as AP
import qualified Data.List.NonEmpty as NE (NonEmpty ((:|)), (<|), head)

tests :: T.TestTree
tests = "ParseGen tests" `T.testGroup` [
    "abnfGrammarTest1" `TH.testCase` do
        parser <- return $ BP.ruleParser ABNF.abnfGrammar (BG.ref "rulelist")
        result <- return . AP.parseOnly parser $ fromString "some-rule = rule1 / rule2\r\n"
        putStrLn $ show $ fmap (BS.simplify . BS.toNode) result
    ]

data Action = SkipAction | ApplyAction
data Movement = MoveDeeper | MoveOn

isCoreRuleNode (BS.RefNode ref _) =
    let matches = filter (\(BG.RuleDecl ref' _) -> ref' == ref) Core.rules
     in not $ null matches
isCoreRuleNode _ = False

walk :: (NE.NonEmpty BS.Node -> (Action, Movement)) -> (NE.NonEmpty BS.Node -> a) -> BS.Node -> [a]
walk strategy action node = reverse $ applyWalk strategy action [] (node NE.:| [])
    where applyWalk strategy action acc nodeStack = 
            let (actionStrat, movement) = strategy nodeStack
                afterAction =
                    case actionStrat of
                        SkipAction -> acc
                        ApplyAction -> (action nodeStack) : acc
                afterMovement =
                    case movement of
                        MoveOn -> afterAction
                        MoveDeeper -> 
                            let deeper = map (\c -> c NE.<| nodeStack) $ BS.getChildren (NE.head nodeStack)
                             in foldl (\a c -> applyWalk strategy action a c) acc deeper
             in afterMovement