module Data.BAByNF.ABNF.Rules.Repeat.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.Repeat qualified as Repeat


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.Repeat"

test_module :: Tasty.TestTree
test_module = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint Repeat.rule @?= "repeat = 1*DIGIT / (*DIGIT \"*\" *DIGIT)"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "2"
    , "15"
    , "*"
    , "15*"
    , "*8"
    , "1*4"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Repeat.ref (stringAsBytesUnsafe s))