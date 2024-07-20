module Data.BAByNF.ABNF.Rules.Elements.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.Elements qualified as Elements

moduleUnderTest :: String
moduleUnderTest = "Test-Data.BAByNF.ABNF.Rules.Elements"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint Elements.rule @?= "elements = alternation *c-wsp"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "opt1 / opt2-1 opt2-1"
    ,  "opt1 / opt2-1 opt2-1 ; comment\r\n "
    , "some-stuff ; comment\r\n "
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Elements.ref (stringAsBytesUnsafe s))