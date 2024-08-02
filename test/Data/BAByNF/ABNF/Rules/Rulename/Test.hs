module Data.BAByNF.ABNF.Rules.Rulename.Test where

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
import Data.BAByNF.ABNF.Rules.Rulename qualified as Rulename


moduleUnderTest :: String
moduleUnderTest = "Data.BAByNF.ABNF.Rules.Rulename"

testModule :: Tasty.TestTree
testModule = Tasty.testGroup moduleUnderTest
    [ testPrettyPrint
    , testParse
    , testParseIntoModel
    ]

testPrettyPrint :: Tasty.TestTree
testPrettyPrint = HUnit.testCase "prettyPrint" $
    prettyPrint Rulename.rule @?= "rulename = ALPHA *(ALPHA / DIGIT / \"-\")"

testParse :: Tasty.TestTree
testParse = Tasty.testGroup "parse" $
    [ "something"
    , "some20word"
    , "simple-lobster"
    ] <&> \s -> HUnit.testCase (show s) $
        either
            (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
            (const $ return ())
            (parse rules Rulename.ref (stringAsBytesUnsafe s))

testParseIntoModel :: Tasty.TestTree
testParseIntoModel = Tasty.testGroup "parseIntoModel" $
    [ ("this-thing-here", Model.Rulename (stringAsBytesUnsafe "this-thing-here"))
    ] <&> \(s, o) -> HUnit.testCase (show s) $
        do
            tree <- either
                (\msg -> HUnit.assertFailure $ "failed to parse provided string with error: [" ++ msg ++ "]")
                return
                (parse rules Rulename.ref (stringAsBytesUnsafe s))
            subTree <- case Tree.asSingleton tree >>= Tree.getSubtreeIfRef Rulename.ref of
                Nothing -> HUnit.assertFailure $ "expected Rulename ref node but result is tree with [" ++ show (length (Tree.nodes tree))  ++ "] children."
                Just subTree -> return subTree
            let model = Rulename.fromTree subTree
            model @?= o