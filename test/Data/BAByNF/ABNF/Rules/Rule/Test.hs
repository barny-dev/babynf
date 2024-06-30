module Data.BAByNF.ABNF.Rules.Rule.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.Rule qualified as Rule


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.Rule"

test_module :: Tasty.TestTree
test_module = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint Rule.rule @?= "rule = rulename defined-as elements c-nl"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "rule = rulename defined-as elements c-nl\r\n"
    , "rule =  rulename defined-as elements c-nl; continues if next line starts\r\n"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Rule.ref (stringAsBytesUnsafe s))