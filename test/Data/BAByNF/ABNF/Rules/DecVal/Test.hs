module Data.BAByNF.ABNF.Rules.DecVal.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.DecVal qualified as DecVal

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.DecVal"

test_module :: Tasty.TestTree
test_module = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint DecVal.rule @?= "dec-val = \"d\" 1*DIGIT [1*(\".\" 1*DIGIT) / (\"-\" 1*DIGIT)]"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "d0"
    , "d9"
    , "d255"
    , "d001-255"
    , "d001.002.090.128"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules DecVal.ref (stringAsBytesUnsafe s))