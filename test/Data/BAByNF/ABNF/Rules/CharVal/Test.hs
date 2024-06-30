module Data.BAByNF.ABNF.Rules.CharVal.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)
import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.CharVal qualified as CharVal

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.CWsp"

test_module :: Tasty.TestTree
test_module = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint CharVal.rule @?= "char-val = case-insensitive-string / case-sensitive-string"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "\"this is a case insensitive string!\""
    , "%i\"this is a case insensitive string!\""
    , "%s\"this is a case sensitive string!\""
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules CharVal.ref (stringAsBytesUnsafe s))