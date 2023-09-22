module Data.BAByNF.Grammar.ABNF.Test (tests) where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as TH

import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Data.ByteString.Char8 as B8
import Data.Either (isRight)

import qualified Data.BAByNF.Grammar as Grammar
import qualified Data.BAByNF.Parsers as Parsers
import qualified Data.BAByNF.Grammar.ABNF as ABNF
import qualified Data.BAByNF.ParseTree as PT


tests :: T.TestTree
tests = "ABNF Grammar Tests" `T.testGroup`
    [ proseValRuleTests
    ]

proseValRuleTests :: T.TestTree
proseValRuleTests = "prose-val tests" `T.testGroup` 
    [ "positive case" `TH.testCase` do
        result <- return $ parse (B8.pack "<this is some free-form text>")
        putStrLn $ show result
        TH.assertBool "Should parse text" (isRight result)
            
    ]
    where parse = Attoparsec.parseOnly (Parsers.ruleParser ABNF.abnfGrammar ABNF.proseValRef)