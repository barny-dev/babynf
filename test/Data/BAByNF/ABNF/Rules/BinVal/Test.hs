module Data.BAByNF.ABNF.Rules.BinVal.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.BinVal qualified as BinVal


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.BinVal"

test_module :: Tasty.TestTree
test_module = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint BinVal.rule @?= "bin-val = \"b\" 1*BIT [1*(\".\" 1*BIT) / (\"-\" 1*BIT)]"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "b0"
    , "b1"
    , "b01010101"
    , "b00001111-11110000"
    , "b00000001.00000010.00000100"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules BinVal.ref (stringAsBytesUnsafe s))