module Data.BAByNF.ABNF.Rules.CaseInsensitiveString.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF qualified as Model
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.CaseInsensitiveString qualified as CaseInsensitiveString


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.CaseInsensitiveString"

test_module :: Tasty.TestTree
test_module = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint CaseInsensitiveString.rule @?= "case-insensitive-string = [\"%i\"] quoted-string"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "\"some string is here\""
    , "%i\"some other string!\""
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules CaseInsensitiveString.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("\"some string\"", Model.CaseInsensitiveString (Model.QuotedString (stringAsBytesUnsafe "some string")))
    , ("%i\"some other string\"", Model.CaseInsensitiveString (Model.QuotedString (stringAsBytesUnsafe "some other string")))
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do 
            tree <- either 
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]") 
                return 
                (parse rules CaseInsensitiveString.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef CaseInsensitiveString.ref of
                Nothing -> HUnit.assertFailure $ "expected CaseInsensitiveString ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]") 
                return
                (CaseInsensitiveString.fromTree subTree)
            model @?= o