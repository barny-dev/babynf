module Data.BAByNF.ABNF.Rules.Option.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.Option qualified as Option


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.Option"

test_module :: Tasty.TestTree
test_module = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint Option.rule @?= "option = \"[\" *c-wsp alternation *c-wsp \"]\""

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "[ref]"
    , "[alt1 / alt2-1 alt2-2]"
    , "[this that]"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Option.ref (stringAsBytesUnsafe s))
