module Data.BAByNF.ABNF.Rules.QuotedString.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF.Model qualified as Model 
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.PrettyPrint
import Data.BAByNF.ABNF.Rules.QuotedString qualified as QuotedString

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.QuotedString"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint QuotedString.rule @?= "quoted-string = DQUOTE *(%x20-21 / %x23-7E) DQUOTE"

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

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("\"this is a quoted string!\"", Model.QuotedString (stringAsBytesUnsafe "this is a quoted string!"))
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do 
            tree <- either 
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]") 
                return 
                (parse rules QuotedString.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef QuotedString.ref of
                Nothing -> HUnit.assertFailure $ "expected QuotedString ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]") 
                return
                (QuotedString.fromTree subTree)
            model @?= o
