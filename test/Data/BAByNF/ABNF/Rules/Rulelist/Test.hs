module Data.BAByNF.ABNF.Rules.Rulelist.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.Rulelist qualified as Rulelist


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.Rulelist"

test_module :: Tasty.TestTree
test_module = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint Rulelist.rule @?= "rulelist = 1*(rule / (*c-wsp c-nl))"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "rule = rulename defined-as elements c-nl\r\n"
    , "rule1 = %x02-0F\r\nrule2 = \"something else\"\r\n"
    , "\r\n"
    , "ruleX = <this> / <that>; some comment\r\nruleY = \"impossible\"\r\n"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Rulelist.ref (stringAsBytesUnsafe s))