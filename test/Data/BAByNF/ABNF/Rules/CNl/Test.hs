module Data.BAByNF.ABNF.Rules.CNl.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.PrettyPrint
import Data.BAByNF.ABNF.Rules.CNl qualified as CNl

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.CNl"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint CNl.rule @?= "c-nl = comment / CRLF"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "\r\n"
    , ";\r\n"
    , "; this is a comment!\r\n"
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules CNl.ref (stringAsBytesUnsafe s))