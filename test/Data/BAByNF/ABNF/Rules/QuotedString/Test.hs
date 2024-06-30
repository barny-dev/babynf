module Data.BAByNF.ABNF.Rules.QuotedString.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)
import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.QuotedString qualified as QuotedString

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.QuotedString"

test_module :: Tasty.TestTree
test_module = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint QuotedString.rule @?= "quoted-string = DQUOTE *(%x20-21 / %x23-7E) DQUOTE"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $ 
    [ "\"\""
    , "\" \""
    , "\"   \""
    , "\"this is a quoted string!\""
    , "\"123456789\""
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules QuotedString.ref (stringAsBytesUnsafe s))
    