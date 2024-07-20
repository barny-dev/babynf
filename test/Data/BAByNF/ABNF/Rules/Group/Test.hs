module Data.BAByNF.ABNF.Rules.Group.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.Group qualified as Group

moduleUnderTest :: String
moduleUnderTest = "Test-Data.BAByNF.ABNF.Rules.Group"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint Group.rule @?= "group = \"(\" *c-wsp alternation *c-wsp \")\""

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "( opt1 / opt2 / opt3 )"
    , "( opt1 ; comment1\r\n opt2 ; comment2\r\n opt3)"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Group.ref (stringAsBytesUnsafe s))