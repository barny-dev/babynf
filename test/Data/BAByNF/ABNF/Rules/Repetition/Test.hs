module Data.BAByNF.ABNF.Rules.Repetition.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.Repetition qualified as Repetition


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.Repetition"

test_module :: Tasty.TestTree
test_module = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint Repetition.rule @?= "repetition = [repeat] element"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "ground-hog"
    , "2ground-hog"
    , "1*ground-hog"
    , "*ground-hog"
    , "*2ground-hog"
    , "*%xFF"
    , "*(some group)"
    , "*\"some text\""
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules Repetition.ref (stringAsBytesUnsafe s))