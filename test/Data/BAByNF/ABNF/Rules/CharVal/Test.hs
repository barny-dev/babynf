module Data.BAByNF.ABNF.Rules.CharVal.Test where

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
import Data.BAByNF.ABNF.Rules.CharVal qualified as CharVal

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.CharVal"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint CharVal.rule @?= "char-val = case-insensitive-string / case-sensitive-string"

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

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("\"some string\"", Model.CaseInsensitiveCharVal $ Model.CaseInsensitiveString (Model.QuotedString (stringAsBytesUnsafe "some string")))
    , ("%i\"some other string\"", Model.CaseInsensitiveCharVal $ Model.CaseInsensitiveString (Model.QuotedString (stringAsBytesUnsafe "some other string")))
    , ("%s\"some string\"", Model.CaseSensitiveCharVal $ Model.CaseSensitiveString (Model.QuotedString (stringAsBytesUnsafe "some string")))
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do 
            tree <- either 
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]") 
                return 
                (parse rules CharVal.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef CharVal.ref of
                Nothing -> HUnit.assertFailure $ "expected CharVal ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]") 
                return
                (CharVal.fromTree subTree)
            model @?= o