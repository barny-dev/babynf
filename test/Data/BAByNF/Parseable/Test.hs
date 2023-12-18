module Data.BAByNF.Parseable.Test (tests) where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as TH

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as B8 ()



tests :: T.TestTree
tests = "ParseGen tests" `T.testGroup` [
    -- "abnfGrammarTest1" `TH.testCase` do
    --     let parser =  Parseable.ruleParser (BG.ref "rulelist")
    --      in do 
    --             print parser
                -- Parseable.parse ABNF.abnfGrammar parser

        -- Pa

        -- parser <- return $ Parseable.parse $ putTrace $ BP.ruleParser ABNF.abnfGrammar (BG.ref "rulelist")
        -- result <- return . AP.parseOnly parser $ fromString "some-rule = rule1 / rule2\r\n"
        -- putStrLn $ show . (fmap (PT.joinStr . (PT.dropRefs Core.ruleRefs))) $ result
    ]
