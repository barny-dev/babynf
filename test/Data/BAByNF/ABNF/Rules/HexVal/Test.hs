module Data.BAByNF.ABNF.Rules.HexVal.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.HexVal qualified as HexVal

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.HexVal"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint HexVal.rule @?= "hex-val = \"x\" 1*HEXDIG [1*(\".\" 1*HEXDIG) / (\"-\" 1*HEXDIG)]"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "x0"
    , "xF"
    , "xFF"
    , "x01-FF"
    , "x01.02.B1.F0"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules HexVal.ref (stringAsBytesUnsafe s))