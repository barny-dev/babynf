module Data.BAByNF.ABNF.Rules.DefinedAs.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.DefinedAs qualified as DefinedAs

moduleUnderTest :: String
moduleUnderTest = "Test-Data.BAByNF.ABNF.Rules.DefinedAs"

test_module :: Tasty.TestTree
test_module = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint DefinedAs.rule @?= "defined-as = *c-wsp (\"=\" / \"=/\") *c-wsp"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "="
    , "=/"
    , " = "
    , " =/ "
    , "; comment\r\n = "
    , "; comment\r\n =/ "
    , "; comment\r\n = ; comment \r\n "
    , "; comment\r\n =/ ; comment \r\n "
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules DefinedAs.ref (stringAsBytesUnsafe s))