module Data.BAByNF.ABNF.Rules.Test (tests) where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as TH
import qualified Data.BAByNF.Util.Ascii as Ascii
import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Data.ByteString.Char8 as B8
import Data.Either (isRight)
import Data.Functor ((<&>))
import qualified Data.BAByNF.ABNF.Grammar as ABNFGrammar
import Data.List.NonEmpty (NonEmpty ((:|)))
-- import qualified Data.BAByNF.Parsers as Parsers
import qualified Data.BAByNF.ABNF.Rules as ABNFRules
import Data.BAByNF.Tree (Tree (..))
import Data.BAByNF.Tree qualified as Tree
import Data.BAByNF.Ref qualified as Ref
import Data.BAByNF.RefDict (RefDict (..))
import Data.BAByNF.Repeat qualified as Repeat
import Data.BAByNF.Parseable
import Data.BAByNF.Parseable qualified as Parseable
import Debug.Trace (trace)
import qualified Data.ByteString.Builder as Attoparsec


putTrace v = trace (show v) v 

tests :: T.TestTree
tests = "ABNF Grammar Tests" `T.testGroup` [
    parseableTests
      ,proseValRuleTests
      ,hexValRuleTests
      ,ruleRuleTests
    ]

parseableTests :: T.TestTree
parseableTests = "parseable tests" `T.testGroup`
    [ let parseable = strUnit "xoxo!"
          parser = Parseable.toParser emptyDict parseable
          result = Attoparsec.parseOnly parser (B8.pack "xoxo!")
       in "#1" `TH.testCase` do
            printResult result
            (Right $ Tree [Tree.StringNode (B8.pack "xoxo!")]) TH.@=? result
    , let parseable = Parseable.Seq $ (strUnit "indoor!") :| [strUnit "xoxo!"]
          parser = Parseable.toParser emptyDict parseable
          result = Attoparsec.parseOnly parser (B8.pack "indoor!xoxo!")
       in "#2" `TH.testCase` do 
            printResult result
            (Right $ Tree [Tree.StringNode (B8.pack "indoor!"), Tree.StringNode (B8.pack "xoxo!")]) TH.@=? result

    , let parseable = Parseable.Rep (strUnit "kelp!") Repeat.once
          parser = Parseable.toParser emptyDict parseable
          result = Attoparsec.parseOnly parser (B8.pack "kelp!")
       in "#3" `TH.testCase` do
            printResult result
            (Right $ Tree [Tree.StringNode (B8.pack "kelp!")]) TH.@=? result

    , let parseable = Parseable.Rep (strUnit "kelp!") Repeat.maybeOnce
          parser = Parseable.toParser emptyDict parseable
          result = Attoparsec.parseOnly parser (B8.pack "kelp!")
       in "#4" `TH.testCase`  do
            printResult result
            (Right $ Tree [Tree.StringNode (B8.pack "kelp!")]) TH.@=? result

    , let parseable = Parseable.Rep (strUnit "kelp!") Repeat.maybeOnce
          parser = Parseable.toParser emptyDict parseable
          result = Attoparsec.parseOnly parser (B8.pack "")
       in "#5" `TH.testCase` do
            printResult result
            (Right $ Tree []) TH.@=? result

    , let parseable = Parseable.Rep (strUnit "kelp!") (Repeat.upTo 2)
          parser = Parseable.toParser emptyDict parseable
          result = Attoparsec.parseOnly parser (B8.pack "kelp!kelp!")
       in "#6" `TH.testCase` do
            printResult result
            (Right $ Tree [Tree.StringNode (B8.pack "kelp!"), Tree.StringNode (B8.pack "kelp!")]) TH.@=? result

    , let parseable = Parseable.Rep (strUnit "kelp!") (Repeat.upTo 2)
          parser = Parseable.toParser emptyDict parseable
          result = Attoparsec.parseOnly parser (B8.pack "kelp!")
       in "#7" `TH.testCase` do
            printResult result
            (Right $ Tree [Tree.StringNode (B8.pack "kelp!")]) TH.@=? result

    , let parseable = Parseable.Rep (strUnit "kelp!") Repeat.zeroOrMore
          parser = Parseable.toParser emptyDict parseable
          result = Attoparsec.parseOnly parser (B8.pack "kelp!kelp!kelp!kelp!")
       in "#8" `TH.testCase` do
            printResult result
            (Right $ Tree [Tree.StringNode (B8.pack "kelp!"), Tree.StringNode (B8.pack "kelp!"), Tree.StringNode (B8.pack "kelp!"), Tree.StringNode (B8.pack "kelp!")]) TH.@=? result

    , let parseable = Parseable.Rep (strUnit "kelp!") Repeat.zeroOrMore
          parser = Parseable.toParser emptyDict parseable
          result = Attoparsec.parseOnly parser (B8.pack "")
       in "#9" `TH.testCase` do
            printResult result
            (Right $ Tree []) TH.@=? result

    , let parseable = Parseable.Alt (strUnit "star!" :| [strUnit "power?"])
          parser = Parseable.toParser emptyDict parseable
          result = Attoparsec.parseOnly parser (B8.pack "star!")
       in "#10" `TH.testCase` do
            printResult result
            (Right $ Tree [Tree.StringNode (B8.pack "star!")]) TH.@=? result
    
    , let parseable = Parseable.Alt (strUnit "star!" :| [strUnit "power?"])
          parser = Parseable.toParser emptyDict parseable
          result = Attoparsec.parseOnly parser (B8.pack "power?")
       in "#11" `TH.testCase` do
            printResult result
            (Right $ Tree [Tree.StringNode (B8.pack "power?")]) TH.@=? result

    , let parseable = rule (ref "wolf") 
          parser = Parseable.toParser (RefDict [(ref "wolf", strUnit "skin")]) parseable
          result = Attoparsec.parseOnly parser (B8.pack "skin")
       in "#11" `TH.testCase` do
            printResult result
            (Right $ Tree [Tree.RefNode (ref "wolf") (Tree [Tree.StringNode (B8.pack "skin")])]) TH.@=? result
    ]
    where strUnit str =  Parseable.Unit "<desc>" ((Attoparsec.string $ B8.pack str) >>= (return . Tree.singleton . Tree.StringNode))
          rule :: Ref.ByteStringRef -> Parseable Ref.ByteStringRef
          rule = Parseable.Rule
          emptyDict :: RefDict Ref.ByteStringRef (Parseable Ref.ByteStringRef)
          emptyDict = RefDict []
          ref = Ref.bytestring . B8.pack
          printResult result = putStrLn $ "\n**** " ++ show result

proseValRuleTests :: T.TestTree
proseValRuleTests = "prose-val tests" `T.testGroup` 
    [ "positive case" `TH.testCase` do
        result <- return $ parse (B8.pack "<this is some free-form text>")
        TH.assertBool "Should parse text" (isRight result)
    ]
    where parse = Attoparsec.parseOnly $ (ABNFGrammar.toParser ABNFRules.abnfGrammar ABNFRules.proseValRef)

hexValRuleTests :: T.TestTree
hexValRuleTests = "hex-val tests" `T.testGroup` 
    [ "positive case 1" `TH.testCase` do
        result <- return $ parse (B8.pack "x1")
        TH.assertBool "Should parse text" (isRight result)
    ,  "positive case 2" `TH.testCase` do
        result <- return $ parse (B8.pack "xa")
        TH.assertBool "Should parse text" (isRight result)
    ,  "positive case 3" `TH.testCase` do
        result <- return $ parse (B8.pack "x1a")
        TH.assertBool "Should parse text" (isRight result)
    ,  "positive case 4" `TH.testCase` do
        result <- return $ parse (B8.pack "xa1-2a")
        TH.assertBool "Should parse text" (isRight result)
    ]
    where parse = Attoparsec.parseOnly $ (ABNFGrammar.toParser ABNFRules.abnfGrammar ABNFRules.hexValRef)

ruleRuleTests :: T.TestTree
ruleRuleTests = "rule tests" `T.testGroup` 
    [ "positive case 1" `TH.testCase` do
        result <- return $ parse (B8.pack "x = xFF\r\n")
        TH.assertBool "Should parse text" (isRight result)
    , "positive case 2" `TH.testCase` do
        result <- return $ parse (B8.pack "x =/ xFF\r\n")
        TH.assertBool "Should parse text" (isRight result)
    ]
    where parse = Attoparsec.parseOnly $ (ABNFGrammar.toParser ABNFRules.abnfGrammar ABNFRules.ruleRef)

