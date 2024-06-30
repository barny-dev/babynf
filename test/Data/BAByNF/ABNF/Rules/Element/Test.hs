module Data.BAByNF.ABNF.Rules.Element.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.Element qualified as Element

moduleUnderTest :: String
moduleUnderTest = "Test-Data.BAByNF.ABNF.Rules.Element"

test_module :: Tasty.TestTree
test_module = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint Element.rule @?= "element = rulename / group / option / char-val / num-val / prose-val"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "some-rule"
    , "\"some char value\""
    , "%i\"some other char value\""
    , "%s\"yet another char value\""
    , "[ something-optional ]"
    , "( alt1 / alt2-1 alt2-2 )"
    , "%xFF"
    , "%x00-FF"
    , "%x01.0A.0A.02.B1"
    , "%d255"
    , "%d000-255"
    , "%d001.010.010.002.177"
    , "%b11111111"
    , "%b00000000-11111111"
    , "%b00000001.00001010.10110001"
    , "<this is prose>"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Element.ref (stringAsBytesUnsafe s))