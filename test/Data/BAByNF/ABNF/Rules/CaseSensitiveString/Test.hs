module Data.BAByNF.ABNF.Rules.CaseSensitiveString.Test where

import Data.Functor ((<&>))

import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Data.BAByNF.Util.Ascii (stringAsBytesUnsafe)

import Data.BAByNF.Core.Tree qualified as Tree

import Data.BAByNF.ABNF qualified as ABNF
import Data.BAByNF.ABNF.Model qualified as Model
import Data.BAByNF.ABNF.Parse (parse)
import Data.BAByNF.ABNF.Rules (rules)
import Data.BAByNF.ABNF.Rules.CaseSensitiveString qualified as CaseSensitiveString

moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.CaseSensitiveString"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest 
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    ABNF.prettyPrint CaseSensitiveString.rule @?= "case-sensitive-string = \"%s\" quoted-string"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "%s\"some string!!!\""
    ] <&> \s -> HUnit.testCase (show s) $ 
        either 
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ()) 
            (parse rules CaseSensitiveString.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("%s\"some string\"", Model.CaseSensitiveString (Model.QuotedString (stringAsBytesUnsafe "some string")))
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do 
            tree <- either 
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]") 
                return 
                (parse rules CaseSensitiveString.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef CaseSensitiveString.ref of
                Nothing -> HUnit.assertFailure $ "expected CaseSensitiveString ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            model <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string into model with error: [" ++ msg ++ "]") 
                return
                (CaseSensitiveString.fromTree subTree)
            model @?= o