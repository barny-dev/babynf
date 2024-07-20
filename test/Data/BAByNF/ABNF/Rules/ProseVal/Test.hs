module Data.BAByNF.ABNF.Rules.ProseVal.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.ProseVal qualified as ProseVal


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.ProseVal"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint ProseVal.rule @?= "prose-val = \"<\" *(%x20-3D / %x3F-7E) \">\""

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "<this is prose!>"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules ProseVal.ref (stringAsBytesUnsafe s))
