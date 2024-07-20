module Data.BAByNF.ABNF.Rules.NumVal.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.NumVal qualified as NumVal

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.NumVal"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint NumVal.rule @?= "num-val = \"%\" (bin-val / dec-val / hex-val)"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "%xFF"
    , "%x01-FF"
    , "%x01.02.B1.F0"
    , "%d255"
    , "%d000-255"
    , "%d001.002.090.128"
    , "%b01010101"
    , "%b00001111-11110000"
    , "%b00000001.00000010.00000100"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules NumVal.ref (stringAsBytesUnsafe s))