module Data.BAByNF.ABNF.Rules.CWsp.Test where

import Data.List qualified as List
import Data.Functor ((<&>), void)

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.CWsp qualified as CWsp

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.CWsp"

test_module :: Tasty.TestTree
test_module = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint CWsp.rule @?= "c-wsp = WSP / (c-nl WSP)"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ " "
    , "\t"
    , "; some comment \r\n "
    , "; some comment \r\n\t"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules CWsp.ref (stringAsBytesUnsafe s))